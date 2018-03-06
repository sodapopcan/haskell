Haskell
=======

These are my scatter brain notes while I teach myself Haskell.  I'm pinning this
repo and updating every day I work on it to try and keep myself honest.

I keep a log right in this README and also check the wiki for sources and notes
on them.

REPL
----

`ghci` is the repl

Set a "nicer" prompt with `:set prompt "> "`

`:l file` to load a file (no need for the `.hs` extension)

`:r` to reload any files loaded with `:l`

`let` to define a name

`:t` to see the type of something

`:i` for info on a type

`:m + <module> [<module> <module> ...]` to include a module or modules

`:k` to see the kind of a type


Log
---

### March 6, 2018

Just read through the rest of LYAH without doing the doing exercises.  I'd
skimmed through it before but it made little sense, of course.  This time
I actually understood most things up until the end.  I'm going to power through
the rest of the book (while recording the examples
[here](https://github.com/sodapopcan/haskell/tree/master/lyahfgg)) and then move
onto [Haskell Book](http://haskellbook.com/) ...whhhiiiich I've already started
(just the first chapter about lambda calculus).

I also cloned and built [this todo
app](https://github.com/cpettitt/haskell-todo) which throws an error on macOS
when trying to list tasks.  Going to look into that.

### March 3, 2018

I've obviously been horrible at keep up with this log. I've been a little
preoccupied with trying to find a new job (my current employer knows I'm looking
so no danger of writing that here) but also been doing plenty of Haskell-related
things.  Working through LYAH gets kind of boring and I keep jumping ahead of
myself, mostly to learn about all the concepts that everyone talks about and
think are scary, namely: functors, monoids, and mondas (and applicatives but not
quite there yet).

I've watched a bunch of videos and particularly watched
[this](https://www.youtube.com/watch?v=ZhuHCtR3xq8&t=3452s) and
[this](https://www.youtube.com/watch?v=t1e8gqXLbsU)
*many* times.  I'm pretty comfortable with the idea that functors are objects
that can be mapped over, but it's important not to think of it like mapping over
a (generic) array.  Of course, mapping over an array (list, in Haskell) is
indeed a functor, other data types can be mapped over as well, like a tree.  The
best way I started to understand functors is that they take a "wrapped" value or
values, unwrap and perform operations on that/those value/s, and return an
object of the same shape.  IE, if you take a tree and pass it to a functor
(being a function), it will be returned as a tree of the same shape with all its
values operated on.

As for monoids, as stated in [Don't Fear the
Monad](https://www.youtube.com/watch?v=ZhuHCtR3xq8&t=3452s), they a set of
things that have rules for combining them and those rules obey some rules (they
also have an identity or "zero" function).  `:info Monid` gives us this:

``` haskell
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
```

So a monoid has an empty value, a way of appending.  It also has a method of
concatanation which you generally get for free with the append method.  The
append method must follow this rule:

``` haskell
(a `append` b) `append` c  ==  a `append` (b `append` c)
```

IE, it doesn't from which side you come from when appending.  Appending c to
b first and then append that to a is no different than appending b to a and then
c to the result of that.

And then they must have a zero value which is a value that when appended won't
change anything.  This is best illustrated in an example:

Whole numbers are monoids and they may be so in two different ways.

One way is for their append operation to be `+` (sum) making their empty value
`0`.  Another way is for them to be appended with `*` (product/multiplication)
which makes the empty value for that to be `1`.  The even more interesting
example from the Don't Fear video is that a 12-hour clock is a monoid.  All the
values can be combined to make a time and `12` is the empty value.  IE, if you
add `12` to 12 o'clock, you get 12 o'clock!

I'm starting to understand mondas as well but I won't get into that now.


### Feb 15, 2018

Been sick and watching a lot of videos.  I've been spending probably too much
time on types over the past few days.  I've watched the [Chris
Allen](https://www.youtube.com/watch?v=p-NBJm0kIYU&t=43s) one twice now and
understanding better and better.  On the topic of LYAH might not be the best
book to begin with, I watched
[this video](https://www.youtube.com/watch?v=02_H3LjqMr8) which moves at
a lightening pace but after the work I've put in so far, it clarified a bunch of
stuff.  I have yet to get a focused stretch of time to power through some
more chapters of LYAH... I've been spending a lot of time really grokking
(ugh, I hate that word) concepts before moving on.

### Feb 11, 2018

Further looked into if I should continue on with LYAH and
decided I'm going to.  The major complaint is that it really just introduces you
to a lot of syntax and doesn't teach you how to build an actual program.  Also
that it has no examples.  This is actually working just fine for me since I'm
rather obsessed with syntax and like to learn as much of it as I can.  Whenever
tutorials tell me "don't worry about what this means for now," I hate that!  As
for the lack of exercises, I've been able to find some elsewhere as I go.  I'm
going to work through LYAH then move onto Chris Allen's book.

At the same time, I find myself constantly looking elsewhere as I read for
clarification.  This hasn't actually bothered me too much, though.

### Feb 9, 2018

Watched [this](https://www.youtube.com/watch?v=DebDaiYev2M) video.  Started on
the types chapter, otherwise, just played around with list comprehensions on the
repl.  Not a super productive session as I didn't have a lot of time.


### Feb 8, 2018

Decided to keep going through LAYH good anyway, but going to be aware of the the
points pointed out int
[this](http://bitemyapp.com/posts/2014-12-31-functional-education.html) blog
post.  I would like to at least finish the second chapter on types and maybe go
on to the next chapter on functions.  Basically I might want to stop when it
starts talking about foreign concepts?  Currying is not a foreign concept but
mondois and monads are.  All I know is that they are a way of dealing with state


### Feb 6, 2018

After re-watching a Chris Allen video I'd watched a couple of years ago and
reading some of his stuff, I found out that he doesn't recommend LYAH for
reasons that seem legit.  I started to read through the cis194 class.  I also
learned I shouldn't use haskell platform and instead use stack.  I think
homebrew just installs stack when you `brew install haskell-platform` anyway...
I'll have to check on that.


### Feb 5, 2018

Started pretty late and made it mostly through list comprehensions in the
Starting Out chapter of LYAH.
