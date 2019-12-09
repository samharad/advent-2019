### Advent of Code 2019
https://adventofcode.com/2019

#### Goals
In order:
* To finish on time
* To broaden and improve my Clojure chops; hopefully, to try out libs like core.async, core.logic
* To write code that is sane enough to keep in a public repo

Last year, I tried to learn Haskell by doing the Advent, which was challenging to say the least; I didn't make it past the 15th day. This year, I intend to finish! To that end, I'm being lenient with my adherence to idioms and best practices; for example, I find myself using threading macros even in situations where they probably [obfuscate the meaning](https://stuartsierra.com/2018/07/06/threading-with-style) for other readers and/or future me. 

#### Findings
As of the first 8 days:

* It's been a joy to solve these problems in Clojure. I attribute this almost entirely to the REPL and the fast feedback loop. For toy problems like this, RDD seems to be a good substitute for TDD.
* I have had no trouble keeping things immutable, whereas I recall having had such trouble last year for efficiency reasons.
* I am thankful not to have to jump through hoops to add side effects to functions, e.g., putting a message on a channel, as I would have to in Haskell.
* The 7th day gave me an excuse to try out core.async with some success, though admittedly I once again underestimated the kind of care that must be taken when introducing asynchronicity into a program; ultimately, I think I ironed out all of the race conditions. Hopefully the coming days will give me another excuse to use this library.


