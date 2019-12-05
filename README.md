# edit-distance-linear

The pure Haskell implementation of the Levenshtein edit distance, with linear space complexity.

## Comparison

There are already several other existing implementations, but the goals and design decisions vary. In particular, this package is intended to be used to:
* compare long strings (think tens of thousands of characters), driving the implementation to live in the `ST` monad and aim at linear space complexity to lower GC pressure;
* not care about Unicode, thus accepting `ByteString`s and comparing them byte-by-byte rather than character-by-character (or glyph-by-glyph, or whatever is the right notion of an edit for Unicode).

Among the alternatives:
* [text-metrics](http://hackage.haskell.org/package/text-metrics) — uses a similar algorithm, but cares about Unicode, making it 4-5 times slower.
* [edit-distance](http://hackage.haskell.org/package/edit-distance) — uses a very different algorithm (which we might implement here one day with huge potential benefits), which tends to consume more memory (I'm not up for estimating its space asymptotics, though).
