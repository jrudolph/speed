# import speed._

:cake: from the hot shores of the black forest :cake:

## What?

**speed** is a compile-time only macro library that provides functionality similar to Scala collection's views
but with an efficient implementation that inlines the complete processing into a (mostly) single while-loop
at the use site.

As an example see this:

```scala
array.speedy.map(x => x * x).foldLeft(0)(_ + _)
```

will be converted into code roughly equivalent to this:

```scala
var acc = 0
var i = 0
while (i < array.length) {
  acc = acc + array(i)
  i += 1
}
counter
```

## Installation

```
resolvers += Opts.resolver.sonatypeReleases

// only needed at compile-time
libraryDependencies += "net.virtual-void" %% "speed" % "15" % "provided"
```

## Usage

There are two principle ways of using **speed**. Either, you explicitly request optimization by
"converting" a collection into a speedy one using `.speedy`. Or, you can also import automatic
conversions in which case supported collections will be auto-optimized (automatic conversion
currently only supported for `Range` and `Array`).

Similar to Java 7 Streams, a **speed** expression must start with a "Generator" like a `Range` or
an `Array`, then apply some non-terminal processing steps, and finally end with a terminal result.

Intermediate results are usually never materialized into a collection and therefore cannot be put
into a variable. Instead, use `to[Coll]` as a terminal step to create a result collection if needed.

### Explicit

Add an import to **speed** in the source file:

```scala
import speed._
```

and then, convert a collection to its speedy variant by adding `.speedy` (similar as you would add `.view`):

```scala
array.speedy.map(_ + 2).sum // this will run fast...
```

### Automatic

Ranges and arrays can also be automatically optimized by `import speed.auto._` at the top of a
source file:

```scala
import speed.auto._

array.map(_ + 2).sum // this will also run fast...
```

## Semantics

Semantics are similar to what to expect from Scala collection's views. However, if in question we'll
take the liberty to divert from the known behavior to allow more aggressive optimizations.

The basic optimization approach is based on aggressive inlining the whole processing pipeline into
a single while-loop.

Apart from that, **speed** aggressively tries to make use of static information in several ways:
  * special cased range loops for the common cases (step = +/- 1)
  * static optimizations over the operations structure to avoid work where possible
  * a final run of constant-folding tries to gather static information

Note: function arguments to `map`, `filter`, etc. are assumed to be pure and to execute no side-effects
to be able to reorder operations for optimizations. You cannot rely on side-effects in these functions
as they may not be executed for **speed** optimized code if the result of the function call isn't needed
for the final result.

## Features

### Supported collection types

 - `Range`
 - `Array`
 - `IndexedSeq`
 - `List`

### Supported operations

**speed** currently supports a subset of interesting and important collection operations from the
Scala collections.

#### Non-Terminal

See the [NonTerminalOps](https://github.com/jrudolph/speed/blob/master/src/main/scala/speed/package.scala) type:
 - flatMap
 - map
 - filter
 - withFilter
 - reverse
 - take
 - (more to come)

#### Terminal

See the [TerminalOps](https://github.com/jrudolph/speed/blob/master/src/main/scala/speed/package.scala) type:

 - foldLeft
 - foreach
 - reduce
 - sum
 - min
 - max
 - count
 - size
 - mkString
 - to[Coll[_]]
 - forall
 - (more to come)

### Optimizations

**speed** already employs some optimizations on the operations structure like these:

 - try to push selection of elements down to the generator (like `take` or `reverse`)
 - implement array operations in terms of range
 - generate three different kind of basic loops for ranges depending on static information
   about `start`, `end`, `step` and `isInclusive` if available

#### Planned

 - for-comprehension pattern extraction support (avoid tuple generation)
 - replace well-known `Numeric` and `Ordering` instances by primitive operations

## How?

In the words of the master of enrichment:

![Yo dawg, we heard you like your scala fast, so we put code in yo' code so it loops while it loops](http://i.imgur.com/fAZMT1J.png)

## Debugging Tools

 - Wrap an expression with `speed.impl.Debug.show` to show the generated code

## Known issues

It is known that **speed** still optimizes side-effects in user code too aggressively away in some cases.
If you encounter such an issue please report it on the issue tracker.

## License

The MIT License

## Hacking

 * Adding support for another collection type: as an example see
   [this commit](https://github.com/jrudolph/speed/commit/b0b0ca9966bd7100d9cdb7b46899dbbcc4b7b196) for how `List` was supported.
 * Adding an optimization:
   see [Optimizer.scala](https://github.com/jrudolph/speed/blob/master/speed/src/main/scala/speed/impl/Optimizer.scala)
   for existing optimizations
 * Adding a (non-)terminal operation:
   see [Generation.scala](https://github.com/jrudolph/speed/blob/master/speed/src/main/scala/speed/impl/Generation.scala)

## Related work

 * [spire's][spire] cfor
 * [scalaxy Streams][scalaxy]
 * [scalablitz][scalablitz]

## Benchmarks

Runtime (smaller is better, reference is an equivalent while loop, "Scala" uses the same
expression with views)

|Description|While|Speedy|Scala|
|-----------|----:|------|----:|
|[foreach counting](#foreach-counting) | 100 % | 102.89 % | 430.06 %
|[nested counting](#nested-counting) | 100 % | 98.63 % | 389.60 %
|[filtered summing](#filtered-summing) | 100 % | 99.40 % | 861.21 %
|[mapped summing](#mapped-summing) | 100 % | 100.15 % | 3985.92 %
|[array foreach counting](#array-foreach-counting) | 100 % | 99.24 % | 450.01 %
|[array summing](#array-summing) | 100 % | 100.23 % | 625.30 %
|[array filtered summing](#array-filtered-summing) | 100 % | 103.45 % | 1137.95 %
|[array mapped summing](#array-mapped-summing) | 100 % | 101.55 % | 2101.77 %
|[size of filtered ref array](#size-of-filtered-ref-array) | 100 % | 98.30 % | 929.55 %
|[array map - filter - to](#array-map---filter---to) | 100 % | 95.01 % | 291.28 %
|[list foreach counting](#list-foreach-counting) | 100 % | 100.07 % | 100.87 %
|[list summing](#list-summing) | 100 % | 104.40 % | 202.86 %
|[list filtered summing](#list-filtered-summing) | 100 % | 104.42 % | 627.96 %
|[list mapped summing](#list-mapped-summing) | 100 % | 100.08 % | 769.58 %
|[nested list summing](#nested-list-summing) | 100 % | 99.52 % | 1133.47 %

(Don't be fooled, values < 100 % are most likely not significant but I'm including them here
 anyways just for the giggles. :laughing:)

(Don't be fooled2, I obviously selected benchmarks in the favor of **speed**, if you'd like to see
 other comparisons please PR.)

Benchmark results can be regenerated by running the `PerformanceSpecs` in the code base.

### Foreach counting

```scala
var counter = 0
for (i ← 1 to 1000) counter += i * i
counter
```

### Nested counting
```scala
var counter = 0
for {
  i ← 1 to 100
  j ← 1 to 100
} counter += i * j
counter
```

### Filtered summing
```scala
(1 to 1000).filter(_ % 3 == 0).sum
```

### Mapped summing
```scala
(1 to 1000).map(i ⇒ i * i).sum
```

### Array foreach counting
```scala
var counter = 0
for (x ← array) counter += x * x
counter
```

### Array summing
```scala
array.sum
```

### Array filtered summing
```scala
array.filter(_ % 3 == 0).sum
```

### Array mapped summing
```scala
array.map(x ⇒ x * x).sum
```

### Size of Filtered ref Array
```scala
class Ref(var num: Int = 0)

val N = 1000
val refs = (0 until N).map(i ⇒ new Ref(i)).toArray

refs
  .filter(_.num % 5 == 0)
  .filter(_.num % 7 == 0)
  .size
```

### Array Map - Filter - To
```scala
array.map(x ⇒ x * x).filter(_ % 3 == 0).to[List]
```
### List foreach counting
```scala
var counter = 0
for (x ← list) counter += x * x
counter
```
### List summing
```scala
list.sum
```
### List filtered summing
```scala
list.filter(_ % 3 == 0).sum
```
### List mapped summing
```scala
list.map(x ⇒ x * x).sum
```
### Nested list summing
```scala
(for (x ← list1; y ← list2) yield x * y).sum
```

[spire]: https://github.com/non/spire
[scalaxy]: https://github.com/ochafik/Scalaxy/tree/master/Streams
[scalablitz]: http://scala-blitz.github.io/

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/fd91a42ebd2f69382ea34cfc2a09d9ed "githalytics.com")](http://githalytics.com/jrudolph/speed)
