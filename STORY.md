# How this got built

It started with VolleyStation. I spent a while experimenting with it, working
out what it actually stores and how it stores it, and realised I could read a
`.dvw` myself. So I wrote a basic HTML page that did exactly that: open a file,
parse the coded touches, put a number on the screen.

The first numbers were the obvious ones. Attacking efficiency. Passing
efficiency. Enough to prove the parsing was right.

Then it got more useful. By rotation. By setter. Hitting by player. Each cut
answered a question a coach was already asking, which is how I picked what to
build next.

Coordinates were the turn. Once I understood how the ball positions were
stored, the question stopped being "what number can I compute" and became "what
can a coach actually read during a match". That is a different problem. A
number is easy. A shot chart that tells you where a hitter goes, in the ten
seconds you have between rallies, is not. I kept building tabs against that
standard.

All of it was still a single HTML file.

Porting it to R and Shiny made it something that can run as a server instead of
a file I email around. That opened up the real target: live stats on the bench,
not a report read the next day. Putting it on GitHub is part of the same move,
somewhere it lives properly and can keep changing.

The goal has not moved. Keep it simple and legible enough that a coach uses it
mid-match without being taught. Find our weaknesses honestly, and find the
other team's. Anything that does not change a decision, in preparation or in
the gym, is not worth the screen space.

Andrew King
Statistics and Data Science, Cal Poly
