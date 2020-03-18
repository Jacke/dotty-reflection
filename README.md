# Dotty Reflection

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/dotty_reflection/images/download.svg)](https://bintray.com/blocke/releases/dotty_reflection/_latestVersion)

Dotty is the exciting new experimental language platform slated to evolve into Scala 3.  One of the big changes for Dotty is that Scala runtime reflection has been eliminated in favor of compile-time reflection, either through macros or Tasty file inspection.  

This project seeks to accomplish two goals:
* Make Dotty reflection a little more approachable by exposing a higher-level abstration for reflected things
* Allow for a runtime reflection capability (i.e. make compile-time appear to work like run-time)

Full disclosure, this project is designed expressly to facilitate Dotty migration of ScalaJack, which is a heavy user of Scala runtime reflection, so the things pulled into the abstraction are driven by ScalaJack's needs.  That said, there's quite a bit there and it may be useful to others.

### Caveats
* This library is highly-experimental and subject to major change/breakage
* As stated, the reflected content is geared for ScalaJack, so your needs may be different

The goal initially is to get this functionality working.  It is not very beautiful.  If you have better ways to do the same thing, please submit a PR!

### Usage
For Tasty Inspection:
```scala
import co.blocke.dotty_reflection

case class Thing(a: String)

val artifact: ConcreteType = Reflector.reflectOn[Thing]
// Concrete type here is typically a ScalaClassInfo or JavaClassInfo but could be something else if you reflected on, say, List[Foo], in which case you'd
// get back a SeqLikeInfo.

// Alternatively, if you have the Class instance:
val art2: ConcreteType = Reflector.reflectOnClass(clazz)
```
From the top-level ConcreteType you get back, you can navigate into the internals of the class, which are themselves reflected items.

### Status
At this point the core Tasty inspection is done, and it inspects quite a lot of things in the Scala ecosystem:
* Dotty/Scala 3 Tasty classes (parameterized or non-parameterized) 
* Traits (including sealed traits)
* Scala 2 case classes
* Java classes (Javabeans pattern)
* Scala 3 enum / Scala 2 Enumeration
* Union & Intersection types
* Opaque type aliases
* Try typed fields
* Either
* Option
* Collections, incl. Java Collections
* Tuple

See unit tests for detailed examples of usage.

### Acknowledgements

I wish to thank three people who have helped make this library possible, with their patient explanations and help on gitter and in code reviews.  Learning the Dotty reflection internals was a learning curve for me and these guys really helped me through it:
```
Guillaume Martres (@smarter)
Paolo G. Giarrusso (@Blaisorblade)
Nicolas Stucki (@nicolasstucki)
```