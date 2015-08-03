# mathjean

Simple scrapers for the Math Genealogy project.
There's an ocaml version and a python version and a Racket version.

## How to use `mathjean.rkt`

- Install Racket v6.2 or so (tested on `6.2.900.5`)
- Install the `sxml` library
- Run `racket mathjean.rkt ID` and wait for the results.
  (The argument `ID` is a number -- the identifier for the mathematician whose ancestors you're searching for. You can also just use a URL.)

## How to use `mathjean.ml`

First compile the executable by running `make`.
You will need a recent version of OCaml and some dependencies.

Now you can run.
1. Find the url of your favorite mathematician. Get his or her ID. For example, the url of John Reppy is `http://genealogy.math.ndsu.nodak.edu/id.php?id=89253` and his ID is `89253`.
2. Run `./ancestors ID` where `ID` is the id you just found.
3. Output will be printed to console. It's an alphabetized set of mathematicians in the tree of your favorite mathematician.

Cool! But beware, this script is VERY fragile.

## How to use `mathjean.py`

You'll first need to install Python 2.7 and Scrapy.

1. Navigate to the url of your favorite mathematician. Save it.
2. Copy the url to the field `start_urls`
3. Let 'er rip: `scrapy runspider mathjean.py`. When prompted, enter the start URL.
4. Output will be saved to `crawl.log`. It's a tab-separated file of student-advisor pairs.

This script is less fragile.

## Alternatives

Both [thearn](https://github.com/thearn/math-genealogy)
and [tzwenn](https://github.com/tzwenn/MathGenealogy)
have much prettier alternatives.

## Closing

This is beerware.
Have fun.
