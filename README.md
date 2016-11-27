# substratum

A Clojure library for implementing a Data Platform on top of Datomic.

Very heavily based on Antonio Andrade's Clojure Conj talk in 2013.
(TODO: Link to that and the associated github repository).

His company provides a data platform that their customers use to build
applications. This means their data model must be extremely flexible.

You probably don't need that sort of flexibility. In case you do,
this is an attempt to replicate the ideas from that talk.

## Basic Idea

The main point is to provide structure to your entity definitions.

In datomic, it's perfectly legal to transact

    [{:db/id #db/id[:db.part/user]
      :person/name "Joe"
      :car/top-speed 200
      :keyboard/layout :us/dv}]

But...what on Earth would those facts be about?

The key ingredient here is defining data types. So you might wind up transacting:

    [{:db/id #db/id[:db.part/user]
      :dt/dt :car
      :car/top-speed 80
      :cylinder/compression-ratio 1.25
      :exterior/color :purple
      :manufactured/inst 1986
      :person/name "Barney"
      :body/style :minivan
      :fuel/efficiency 16.2
      :make :chevy
      :model :astro}]

if you want to remember the most awesome vehicle on the planet.

The key datom that makes this transaction different than the first is the
:dt/dt attribute. If you're coming from an RDBMS background, it helps to think
of that as the table where this entity lives. If you're coming from an OO
background, the acronym is for Data Type.

If you're coming at this from a dynamically typed functional programming
background, just think of it as a tag to help you remember what's supposed to be in the
map.

Note that you're responsible for defining what fields are associated with
any given data type. And for deciding how strict you want to be about
it. Maybe assigning :person/name to a car is a bug. Maybe it's a deliberate
quirk associated with this particular entity, and we have to give it a
pass.

## TODO: Write up real Usage

At this point, everything's so flakey that I can't remember most of the points
behind the way I hoped I'd be able to use this. So you should probably steer well
clear until I can remember the basics, at least.

It looks like I was very much trying to build on the work of YuppieChef and
Ryan Kneufield in datomic-schema and conformity to keep this compact.

## License

Copyright © 2016 James Gatannah

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
