# ---------------------------------------------------------------------------
# A QR encoder, in base R
#
# So an iPad can be pointed at the laptop and get the address, which is what
# VolleyStation does and the fastest way to get five tablets onto a match.
#
# Written out rather than taken from a package because of where this runs. On
# match day the laptop is on a router with no internet, so a missing package
# cannot be installed in the gym, and the project otherwise needs nothing but
# shiny and data.table. A URL is a small enough payload that the encoder fits
# in a file.
#
# Byte mode, error correction level M, versions 1 to 6, which is up to 106
# bytes and far more than any "http://name.local:7815" needs. Stopping at 6
# keeps two awkward parts of the standard out: version information blocks
# start at version 7, and the error correction blocks are all one size at this
# level below it. Longer input returns NULL and the caller shows the address
# as text.
#
# Checked module for module against the Python `qrcode` package, which is an
# independent implementation: 416 of 416 matrices identical, over versions 1
# to 6, all eight masks, and every URL shape this app produces. The matrices
# this agreement covers are kept as fixtures in tests/verify.R, so the check
# still runs in a gym with no internet and no Python.
#
# One thing found while checking and left alone. OpenCV's QR detector misses
# some of these symbols above a certain image scale, including the one for
# this laptop's own address, while reading the same symbol happily at smaller
# scales. The matrix is byte for byte what the reference encoder produces for
# that mask, and the mask is the one the standard's penalty rules select, so
# the symbol is right and the detector is the weak part. Phone cameras do not
# use that detector. Worth knowing if a scan ever misbehaves: try a different
# size before suspecting the code.
# ---------------------------------------------------------------------------

# Total data codewords and EC codewords per block, level M, versions 1 to 6.
# Every one of these is a single group of equally sized blocks, which is why
# the interleave below does not need the two-group case.
QR_SPEC <- list(
  list(ver = 1, blocks = 1, data_cw = 16, ec_cw = 10),
  list(ver = 2, blocks = 1, data_cw = 28, ec_cw = 16),
  list(ver = 3, blocks = 1, data_cw = 44, ec_cw = 26),
  list(ver = 4, blocks = 2, data_cw = 64, ec_cw = 18),
  list(ver = 5, blocks = 2, data_cw = 86, ec_cw = 24),
  list(ver = 6, blocks = 4, data_cw = 108, ec_cw = 16)
)

# Centres of the alignment patterns. Version 1 has none.
QR_ALIGN <- list(integer(), c(6, 18), c(6, 22), c(6, 26), c(6, 30), c(6, 34))

# ---- GF(256) ---------------------------------------------------------------
# The field the Reed-Solomon codes live in, x^8 + x^4 + x^3 + x^2 + 1.

qr_gf <- local({
  exp_t <- integer(512); log_t <- integer(256)
  x <- 1L
  for (i in 0:254) {
    exp_t[i + 1L] <- x
    log_t[x + 1L] <- i
    x <- bitwShiftL(x, 1L)
    if (x >= 256L) x <- bitwXor(x, 0x11DL)
  }
  for (i in 255:511) exp_t[i + 1L] <- exp_t[i - 255L + 1L]
  list(exp = exp_t, log = log_t)
})

gf_mul <- function(a, b) {
  if (a == 0L || b == 0L) return(0L)
  qr_gf$exp[(qr_gf$log[a + 1L] + qr_gf$log[b + 1L]) %% 255L + 1L]
}

#' The generator polynomial for `n` error correction codewords
gf_generator <- function(n) {
  g <- 1L
  for (i in 0:(n - 1L)) {
    # multiply g by (x - alpha^i)
    ng <- integer(length(g) + 1L)
    a <- qr_gf$exp[i + 1L]
    for (j in seq_along(g)) {
      ng[j] <- bitwXor(ng[j], g[j])
      ng[j + 1L] <- bitwXor(ng[j + 1L], gf_mul(g[j], a))
    }
    g <- ng
  }
  g
}

#' The `n` error correction codewords for one block
qr_ec <- function(data, n) {
  g <- gf_generator(n)
  rem <- c(data, integer(n))
  for (i in seq_along(data)) {
    lead <- rem[i]
    if (lead == 0L) next
    for (j in seq_along(g))
      rem[i + j - 1L] <- bitwXor(rem[i + j - 1L], gf_mul(g[j], lead))
  }
  rem[(length(data) + 1L):length(rem)]
}

# ---- bits ------------------------------------------------------------------

bits_of <- function(value, n) as.integer(bitwAnd(bitwShiftR(value, (n - 1L):0L), 1L))

#' Mode indicator, length, payload, terminator, padding
qr_bitstream <- function(bytes, spec) {
  total <- spec$data_cw * 8L
  b <- c(bits_of(4L, 4L),            # byte mode
         bits_of(length(bytes), 8L), # count, 8 bits for versions 1 to 9
         unlist(lapply(bytes, bits_of, n = 8L)))
  b <- c(b, integer(min(4L, total - length(b))))        # terminator
  if (length(b) %% 8L) b <- c(b, integer(8L - length(b) %% 8L))
  pad <- c(0xECL, 0x11L)
  i <- 1L
  while (length(b) < total) {
    b <- c(b, bits_of(pad[i], 8L))
    i <- if (i == 1L) 2L else 1L
  }
  b
}

#' Data and EC codewords, interleaved as the standard orders them
qr_codewords <- function(bytes, spec) {
  bits <- qr_bitstream(bytes, spec)
  cw <- vapply(seq(1L, length(bits), by = 8L),
               function(i) sum(bits[i:(i + 7L)] * 2^(7:0)), 0)
  cw <- as.integer(cw)
  per <- spec$data_cw / spec$blocks
  dat <- lapply(seq_len(spec$blocks), function(k) cw[((k - 1L) * per + 1L):(k * per)])
  ecc <- lapply(dat, qr_ec, n = spec$ec_cw)
  # Column by column across the blocks, data first, then EC.
  out <- integer(0)
  for (i in seq_len(per))  out <- c(out, vapply(dat, function(d) d[i], 0L))
  for (i in seq_len(spec$ec_cw)) out <- c(out, vapply(ecc, function(e) e[i], 0L))
  out
}

# ---- the matrix ------------------------------------------------------------

#' Finders, separators, timing, alignment and the reserved areas
#'
#' Returns the module values and, beside them, which cells are function
#' patterns. Data is only written where `fixed` is FALSE, and the mask is only
#' applied there too.
qr_frame <- function(ver) {
  n <- 17L + 4L * ver
  m <- matrix(0L, n, n)
  fixed <- matrix(FALSE, n, n)

  put <- function(r, c, v) { m[r, c] <<- v; fixed[r, c] <<- TRUE }

  finder <- function(r0, c0) {
    for (dr in 0:6) for (dc in 0:6) {
      # 3x3 solid centre, a white ring around it, a black border outside
      d <- max(abs(dr - 3L), abs(dc - 3L))
      put(r0 + dr, c0 + dc, if (d == 2L) 0L else 1L)
    }
  }
  sep <- function(rs, cs) for (r in rs) for (c in cs)
    if (r >= 1L && r <= n && c >= 1L && c <= n) put(r, c, 0L)

  finder(1L, 1L);          sep(1:8, 8);   sep(8, 1:8)
  finder(1L, n - 6L);      sep(1:8, n - 7L); sep(8, (n - 7L):n)
  finder(n - 6L, 1L);      sep((n - 7L):n, 8); sep(n - 7L, 1:8)

  for (i in 9:(n - 8L)) { put(7L, i, (i %% 2L)); put(i, 7L, (i %% 2L)) }

  ctr <- QR_ALIGN[[ver]]
  if (length(ctr)) {
    for (rc in ctr) for (cc in ctr) {
      # skip the three that would sit on a finder
      if ((rc == 6L && cc == 6L) || (rc == 6L && cc == max(ctr)) ||
          (rc == max(ctr) && cc == 6L)) next
      for (dr in -2:2) for (dc in -2:2)
        put(rc + 1L + dr, cc + 1L + dc,
            if (max(abs(dr), abs(dc)) == 1L) 0L else 1L)
    }
  }

  put(n - 7L, 9L, 1L)                      # the dark module

  # Reserve the format information areas; the values go in later.
  for (i in 1:9) { if (!fixed[9L, i]) put(9L, i, 0L); if (!fixed[i, 9L]) put(i, 9L, 0L) }
  for (i in 1:8) { if (!fixed[9L, n - 8L + i]) put(9L, n - 8L + i, 0L) }
  for (i in 1:7) { if (!fixed[n - 7L + i, 9L]) put(n - 7L + i, 9L, 0L) }

  list(m = m, fixed = fixed)
}

#' Walk the zigzag and drop the codeword bits in
qr_place <- function(m, fixed, cw) {
  n <- nrow(m)
  bits <- unlist(lapply(cw, bits_of, n = 8L))
  i <- 1L
  col <- n
  upward <- TRUE
  while (col >= 1L) {
    if (col == 7L) col <- col - 1L          # the vertical timing column
    rows <- if (upward) n:1 else 1:n
    for (r in rows) for (c in c(col, col - 1L)) {
      if (c < 1L || fixed[r, c]) next
      m[r, c] <- if (i <= length(bits)) bits[i] else 0L
      i <- i + 1L
    }
    col <- col - 2L
    upward <- !upward
  }
  m
}

QR_MASK <- list(
  function(r, c) (r + c) %% 2 == 0,
  function(r, c) r %% 2 == 0,
  function(r, c) c %% 3 == 0,
  function(r, c) (r + c) %% 3 == 0,
  function(r, c) (r %/% 2 + c %/% 3) %% 2 == 0,
  function(r, c) (r * c) %% 2 + (r * c) %% 3 == 0,
  function(r, c) ((r * c) %% 2 + (r * c) %% 3) %% 2 == 0,
  function(r, c) ((r + c) %% 2 + (r * c) %% 3) %% 2 == 0
)

#' The four penalty rules, which decide which mask is used
qr_penalty <- function(m) {
  n <- nrow(m)
  p <- 0L

  run_penalty <- function(v) {
    r <- rle(v); k <- r$lengths[r$lengths >= 5L]
    if (length(k)) sum(k - 2L) else 0L
  }
  for (i in 1:n) p <- p + run_penalty(m[i, ]) + run_penalty(m[, i])

  blocks <- m[1:(n - 1L), 1:(n - 1L)] + m[2:n, 1:(n - 1L)] +
            m[1:(n - 1L), 2:n] + m[2:n, 2:n]
  p <- p + 3L * sum(blocks == 0L | blocks == 4L)

  pat1 <- c(1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0)
  pat2 <- rev(pat1)
  hits <- function(v) {
    if (length(v) < 11L) return(0L)
    s <- vapply(seq_len(length(v) - 10L), function(i) {
      w <- v[i:(i + 10L)]
      identical(as.numeric(w), pat1) + identical(as.numeric(w), pat2)
    }, 0)
    sum(s)
  }
  for (i in 1:n) p <- p + 40L * (hits(m[i, ]) + hits(m[, i]))

  dark <- sum(m) / (n * n) * 100
  p + 10L * as.integer(abs(dark - 50) %/% 5)
}

#' The 15 format bits: level M, the mask, BCH check, then the fixed XOR
qr_format_bits <- function(mask) {
  v <- bitwOr(bitwShiftL(0L, 3L), mask)       # 00 is level M
  d <- bitwShiftL(v, 10L)
  for (i in 14:10)
    if (bitwAnd(bitwShiftR(d, i), 1L)) d <- bitwXor(d, bitwShiftL(0x537L, i - 10L))
  bits_of(bitwXor(bitwOr(bitwShiftL(v, 10L), d), 0x5412L), 15L)
}

qr_put_format <- function(m, mask) {
  n <- nrow(m)
  f <- qr_format_bits(mask)
  # copy one: around the top left finder
  pos1 <- list(c(9, 1), c(9, 2), c(9, 3), c(9, 4), c(9, 5), c(9, 6), c(9, 8),
               c(9, 9), c(8, 9), c(6, 9), c(5, 9), c(4, 9), c(3, 9), c(2, 9),
               c(1, 9))
  # copy two: below the top right finder and right of the bottom left one
  pos2 <- c(lapply(0:6, function(i) c(n - i, 9L)),
            lapply(0:7, function(i) c(9L, n - 7L + i)))
  for (i in 1:15) {
    m[pos1[[i]][1], pos1[[i]][2]] <- f[i]
    m[pos2[[i]][1], pos2[[i]][2]] <- f[i]
  }
  m
}

#' Encode a string as a QR matrix
#'
#' Returns a 0/1 matrix with no quiet zone, or NULL if the text does not fit
#' in version 6 at level M. The caller adds the quiet zone, which has to be
#' four modules wide or a phone will not see the code.
qr_encode <- function(text) {
  bytes <- as.integer(charToRaw(enc2utf8(as.character(text))))
  spec <- NULL
  for (s in QR_SPEC) if (length(bytes) <= s$data_cw - 2L) { spec <- s; break }
  if (is.null(spec)) return(NULL)

  cw <- qr_codewords(bytes, spec)
  fr <- qr_frame(spec$ver)
  base <- qr_place(fr$m, fr$fixed, cw)

  n <- nrow(base)
  rr <- row(base) - 1L
  cc <- col(base) - 1L
  best <- NULL; best_p <- Inf
  for (k in 0:7) {
    sel <- QR_MASK[[k + 1L]](rr, cc) & !fr$fixed
    cand <- qr_put_format(ifelse(sel, 1L - base, base), k)
    p <- qr_penalty(cand)
    if (p < best_p) { best_p <- p; best <- cand }
  }
  best
}

#' A QR matrix as an inline SVG
#'
#' One path of rectangles rather than one element per module, which keeps a
#' version 3 code to a few kilobytes, and shape-rendering crispEdges so the
#' modules do not blur into each other.
#'
#' The width is rounded to a whole number of pixels per module rather than
#' taken as given. A 37-module symbol asked to be 150px wide would put 4.05
#' pixels in each module, and the half pixel at every edge is exactly the
#' blur a camera has to work through. `px` is treated as the size wanted and
#' the nearest size that divides cleanly is used instead.
qr_svg <- function(text, px = 150, quiet = 4L, alt = NULL) {
  m <- qr_encode(text)
  if (is.null(m)) return("")
  n <- nrow(m)
  side <- n + 2L * quiet
  px <- max(3L, round(px / side)) * side
  d <- character()
  for (r in 1:n) {
    row_v <- m[r, ]
    c0 <- 1L
    while (c0 <= n) {
      if (row_v[c0] == 1L) {
        c1 <- c0
        while (c1 < n && row_v[c1 + 1L] == 1L) c1 <- c1 + 1L
        d <- c(d, sprintf("M%d %dh%dv1h-%dz",
                          c0 - 1L + quiet, r - 1L + quiet,
                          c1 - c0 + 1L, c1 - c0 + 1L))
        c0 <- c1 + 1L
      } else c0 <- c0 + 1L
    }
  }
  sprintf(paste0('<svg class="qr" viewBox="0 0 %d %d" width="%d" height="%d" ',
                 'shape-rendering="crispEdges" role="img" aria-label="%s">',
                 '<rect width="%d" height="%d" fill="#fff"/>',
                 '<path d="%s" fill="#111"/></svg>'),
          side, side, px, px,
          if (is.null(alt)) "QR code for the address" else alt,
          side, side, paste(d, collapse = ""))
}
