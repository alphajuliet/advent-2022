# Advent of Code 2022

More Advent-ures in code, again using Clojure and some preferred libraries for dealing with matrices, graphs, combinatorics, parsing, and so on. I have some grand plans about using some other languages on certain problems, or as an alternative solution, but I'm not sure I will end up doing that very much. BQN is a language of interest, for example, but I'm only going there if an array language will really get me there faster or significantly more elegantly. However, I'm no expert in BQN and I don't want to be writing, say, low-level parsing functions when I have a perfectly good Clojure parser at my fingertips.

I will comment on some of the solutions as I go, but not everything like I've done before.

## Commentary

Days 1 and 2 are gently easing everyone into the game, but even day 2 will be testing the very casual coders to pick a data structure (or not) that captures the rules and scoring. I went with a couple of 3x3 matrices and kept it simple, possibly at the expense of some elegance. But there are no prizes for style in this challenge, it's just stars for right answers.

Days 3 and 4 slowly raise the bar. For both of these I used Clojure's set functions to identify common and different elements but there are a number of ways of doing it. As is often the case, wrangling the data into the right format is a significant part of the effort. Selecting that data structure is also a key decision for what comes after.

And then Day 5 comes along with a step change in difficulty. For a start, the input data needs a bunch of work to get it into something useful. It's possibly overkill but I pulled out my trusty parser (`instaparse`), not because I couldn't wrangle regular expressions but because it makes it more obvious what's going on. It's also good practice to write a simple grammar because I guarantee I'm going to need it again soon. Once you have the input in a tractable form, it's a state machine exercise to step through each instruction and update the stacks. Part 2 just changes the operation. This one could perhaps be shortened some more, but it's pretty clean, so I'll leave it.

Day 6 is easy. Day 7 is tricky. I know I need to create a tree structure, and I have a nicely parsed representation of the input data, and going from the latter to the former is eluding me in Clojure. I've hit a mental block on this one so it goes unsolved.

Days 8, 9, and 10 are more straightforward. Nothing too crazy here. Day 10 is executing a little processor, something we've seen before.


## Licence

Copyright 2022 Andrew Joyner.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
