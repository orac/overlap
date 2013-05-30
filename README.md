# Find words in common between word lists

One evening a non-compsci friend wondered how one might find out what words are
shared by two given languages; in fact, he generalised the question to words
shared by all of *n* given languages. Recalling that Unix-like machines come
armed with dictionary lists in the one-word-per-line format, in files in
`/usr/share/dict/`, I thought that the problem sounded like the trivial sort of
thing one gives as an exercise to first-years. I set about writing a short
program to compute the list of shared words; that is, the intersection of the
lists.

The trick to the program is to know that the dictionary lists are stored in
alphabetical order. Thus, one can find the intersection between two lists in
O(n) time as follows:

## Idealised two-list algorithm

* Have a "working item" taken from the head of each list
* Compare the two working items. If they're equal, output the item, and then
advance both lists. If they're unequal, discard the one that sorts earlier, and
advance that list; keep hold of the other.
* Either way, you've removed an item from one or both lists. If both lists have
further elements, (recursively) repeat the above procedure.
* When either list becomes empty, halt.

The Haskell implementation is shorter than the English-language description, and
no more complicated.
There is, of course, a little necessary code (in the `IO` monad) to deal with
reading and splitting the lists, and printing the output. If you wanted to
develop the program further, a usage message would be helpful, as would better
error-handling if an input file doesn't exist, can't be read, etc. This proves
to be more code than the algorithm itself.

## Many lists

The idealised algorithm generalises to many input lists in the obvious way:
compare all the working items, and discard any that don't sort latest (that is,
all the ones you don't discard are equal). An alternative generalisation is to
run the algorithm on two lists, then on the intermediate result and one further
input list, then on that result and one further input list, and so on until all
lists have been intersected. This is possible because the algorithm keeps the
output in the same sorted order that the inputs were in.

In a lazy language like Haskell, both those alternatives are equivalent, since
an element of the intermediate list is only computed when required for the
comparison. Thus the Haskell implementation of the generalised algorithm is
`foldl1` applied to the two-way algorithm. (`foldl` wouldn't make sense here,
because the 'neutral' list would be the list of all possible strings. Instead,
I special-case the no-input-lists case to return no output.)

## Complications
Of course, like any idealised algorithm applied to the real world, it's not that
simple. My original implementation had two problems.

### I before e
Alphabetical order is not the same in every language. For example, the English
word list has é sorted after z (in so-called ASCIIbetical order), whereas the
French list has it sorted equal to e.

The workaround for this problem is to sort the list of words after splitting up
the file, which means the runtime is no longer O(n), and that the whole file has
to be read at the start instead of processing as it's read. It's still more
efficient to do this for an up-front sort cost than to use the `intersect`
built-in, which has to do a linear look-up for every item of every list.

### Just in case
Spelling engines for different languages use case differently. For example, the
whole French word list is lower-case. The English word list has proper nouns and
abbreviations with upper-case characters (and sorts all upper-case characters
before all lower-case characters). The German word list has all nouns
capitalized, the way German is usually written.

The workaround for this problem is to convert each letter to upper-case. This is
more complicated than it sounds when dealing with multiple languages. For
example, the German letter ß capitalizes to SS, which is two letters. (This
change isn't reversible: `toLower "SS"` evaluates to `"ss"`.) In fact, the
locale mechanism specifies different case rules for each language, which my
program ought to respect, but doesn't, because for languages that can have
overlapping words it doesn't make much difference.

This workaround potentially introduces a new problem: if the same word is
present in a language file in different cases, it will become duplicated. Thus
it's necessary in theory to de-duplicate the list after (or as part of) sorting.
I didn't do this because I considered the issue unlikely to arise in this
context, but it's easy enough to add: in fact, I have given this very exercise
to first-year compscis.

Even without the extra step, decoding the UTF-8 input files into Unicode strings
adds significant overhead to the program compared with my original solution of
treating everything as a bytestring (using the Haskell package of the same
name). It slows the runtime by a factor of three or four (depending on workload
and cache state).

### Character assassination
We've already seen problems with characters that exist in some languages and not
others, and if you think about that, you'll notice I've completely ignored a
huge limitation. Sometimes the words that are the same in two languages actually
are loanwords that have been *transliterated* (such as "karaoke"). To do the
specific job that was originally suggested, rather than just comparing
word-lists, the program ought to have a function for each language that
transliterates each word to a standard representation. This function would take
the place of `toUpper` when reading in the word list, but computing such a
function for each language is left as an exercise for the interested student. As
things stand, the results for European languages (which share most of their
characters) are pretty good.

# LICENSE

Copyright © 2013 Daniel Hulme

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
