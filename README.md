# import speed._

:cake: from the hot shores of the black forest :cake:

## What?

**speed** is a compile-time only macro library that provides functionality similar to Scala collection's views
but with an efficient implementation that inlines the complete processing into a (mostly) single while-loop
at the use site.

## Installation

```
resolvers += Opts.resolver.sonatypeReleases

// only needed at compile-time
libraryDependencies += "net.virtual-void" %% "speed" % "15" % "provided"
```

## Usage

There a two principle ways of using **speed**. Either, you explicitly request optimization by
"converting" a collection into a speedy one using `.speedy`. Or, you can also import automatic
conversions in which case supported collections will be auto-optimized (automatic conversion
currently only for `Range` and `Array`).

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

### Collection types

 - `Range`
 - `Array`
 - `IndexedSeq`
 - `List`

### Operations

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

## Related work

 * [spire's][spire] cfor
 * [scalaxy Streams][scalaxy]
 * [scalablitz][scalablitz]

## Benchmarks

Runtime (smaller is better, reference is an equivalent while loop)

|Description|While|Speedy|Scala|
|-----------|----:|------|----:|
|[foreach counting](#foreach-counting) | 100 % | 100.22 % | 135.97 %
|[nested counting](#nested-counting) | 100 % | 100.91 % | 218.19 %
|[filtered summing](#filtered-summing) | 100 % | 98.22 % | 1543.41 %
|[mapped summing](#mapped-summing) | 100 % | 102.32 % | 1585.62 %
|[array foreach counting](#array-foreach-counting) | 100 % | 99.47 % | 443.93 %
|[array summing](#array-summing) | 100 % | 99.73 % | 296.28 %
|[array filtered summing](#array-filtered-summing) | 100 % | 103.16 % | 973.61 %
|[array mapped summing](#array-mapped-summing) | 100 % | 100.69 % | 450.02 %
|[size of filtered ref array](#size-of-filtered-ref-array) | 100 % | 99.83 % | 351.10 %

(Don't be fooled, values < 100 % are most likely not significant but I'm including them here
 anyways just for the giggles. :laughing:)

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

[spire]: https://github.com/non/spire
[scalaxy]: https://github.com/ochafik/Scalaxy/tree/master/Streams
[scalablitz]: http://scala-blitz.github.io/

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/fd91a42ebd2f69382ea34cfc2a09d9ed "githalytics.com")](http://githalytics.com/jrudolph/speed)
