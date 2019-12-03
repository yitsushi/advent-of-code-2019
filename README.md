# Advent of Code 2019

![Pinky and ](.assets/pinky-and-the-brain.jpg)

> Pinky: Gee, Brain. What are we going to do in December?
>
> The Brain: The same thing we do every year, Pinky. Try to take over the world.

This year, I decided to complete the whole AoC challenge in Haskell.
But why? Why not? In the last few days I started to experiment with
Haskell a bit and I can say, I really like it. It's not natural
as Go, PHP or Python for me (yet), but two years ago C++ was not
comfortable during the first few days, but I came out with a much
better understanding of the language at the end. So that's
my expectation: "after day 25 I can say, I know more and it was fun"

## Before December

I was not sure if I really want to do this, but I say, if I can
make a good-enough framework in a few hours, then let's do this
in Haskell. And well, I did something. Feel free to ping me if
you see something, I really like to learn from this. I'm not
a Haskell developer, I barely know the language and my very first
hello word application was written like a week ago, and if you are
a Haskell developer and even you have a bit more knowledge than me
(it's not a hard task to be honest) then all notes or just links
are welcomed.

## Build

### Example with stack run

```
$ stack run -- --day=0 --part=2 --input=input/demo-2019-day01
```

### Build everything and use

```
$ stack build
$ stack exec -- advent-of-code-2019 --day=0 --part=1 --input=input/demo-2018-day01
```

### Build and install ;)

Why would you do that XD

```
$ stack install
$ advent-of-code-2019 --day=0 --part=1 --input=input/demo-2018-day01
```

### Tests

```
$ stack test
```

For a specific day only

```
$ stack test --ta "-m Day03"
```

# History

 - [2018] => Python
 - [2017] => [aoc-random-language]
 - [2016] => C++

[aoc-random-language]: https://github.com/Yitsushi/aoc-random-language
[2018]: https://github.com/yitsushi/advent-of-code-2018
[2017]: https://github.com/yitsushi/advent-of-code-2017
[2016]: https://github.com/yitsushi/advent-of-code-2016
