# Set #

This is the game Set. See:
[https://en.wikipedia.org/wiki/Set_(game)]

## To play ##

Download the file set.html (in the src directory) and the directory src/img. To play open set.html in any browser.

## Things to do ##

List of things I want to do with it, in no particular order:


* Put an Against the machine (with difficulty) (it will give you a fixed amount of time to find a set before the point is taken for the machine). I will need to think how to do this.
* The options for size don't work on mobile phones (at least in mine)

I think that is all.


This is going to prove difficult, since the time requiring for the algorithm I thoght is too big
* Create a Hint button (with a Set detected in the table highlights some card)
* Automate the AddMoreCards button (it should detect if there is a Set in the table and if not add automatically)


## Things done ##

* Change the type of Cards, to (Int,Int,Int,Int), or actually something more
precise.
* Put an easy version (with only one color for colorblinds and/or children)
* Change the card gifs so that the colors are more similar (I don't like the current ones)
* Put everything with relative size so that it can always be seen on any
screen. (Now there is an option for choosing the size)
* Automate Set button (so that with the third selection takes the set, or gives you a message of error)
* The code needs more cleaning: separate into different modules.
* Separate the different modes of play. Normal, With Clock, And One color normal, and with clock.
* For that I really need to clean the code.
* Add best time in current plays
* Put the rules of the game in the start page.
