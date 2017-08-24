# Introduction to substratum

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

This is a library that's based heavily on a great Clojureconj
2013 talk by
Antonio Andrade, titled "[Building a Data Platform with
Datomic](https://www.youtu.com/watch?v=sQCoTu5v1Mo)."

Substratum's main purpose in life is to set up a system where you host
logical databases that act as a platform for building lots of
disjoint applications.

These are my notes from that talk (quite a bit of this is straight
off his slides).

## Q: What is a Data Platform?

Software system that defines a **data representation**, abstracts
its **persistence** and **retrieval**, and provides tools to
**manipulate** that data.

## Data Models are at the core of Data Platforms

If you get the Data Model wrong, you're screwed.

### What's a Data Model?

Describe the **structure** of the *objects* represented by a system
and the **relationship** between those *objects*.

* Always exists, either implicitly or explicitly
* Really valuable as a communication tool and documentation
* Schema is a *language* used to describe them

It's a mental model of objects and their relationships

### Schema describes the Data Model

It's great for conveying "what pieces do we have now?"

It's awful when structure needs to change.

#### Key to solving that: adaptability

Application code should be able to retrieve the data in whatever
shape is needed.

System should not require live data migration to reshape
the structure of the data when the structure needs to evolve.

Avoid systems that can only serve efficient queries retrieving
data in the shape in which they were originally stored.

This is where document stores and schema-less databases fall apart.

## Enforce Arbitrary Data Constraints

### TODO: Typed references

### Enums

e.g. :person/gender, :person.gender/female, :person.gender/male,
:person.gender/other

Can assign :person/gender the value :car/top-speed

You can fix this with a database function:

TODO: Switch to an editor that lets me copy/paste this in

## Reified Transactions

Attach information to actual transactions

Audit trail

    [[:db/retract 12345 :health/insurance
      [:db/add (new-id :db.part/tx) :audit/source username-id]]]

## Schema Changes

This is where life gets much more complicated

dt.meta/required - vital boolean attribute that needs to be defined
and associated with some fields. But really only for some datatypes

Fix invalid schemas with database functions

Database functions are still attributes.

Every time you add a required attribute, you must specify a
migration function.

Enforce constraints when new entities are created.

i.e. wrap (d/transact) and have it perform constraint checks

### Biggest Problem: When to Migrate

#### Could do it at query time.

But then [something] cannot be used as a rule (makes life
more complicated...this is a limitation baked into datomic)

#### Transaction Time

But then queries could return "invalid" data

#### Attribue Access Time

Very restricted scope, and gets ugly quickly.

i.e. Every time you check for health insurance, add the
default if it's missing.

#### Background Processing Task(s)

You wind up with eventual consistency, which fights
datomic's design.

#### Manually inside application code

## Raw low-level datoms don't visualize well

At least, not the grid-stuff everyone expects

It's probably similar to problems I've seen visualizing
complex graphs.
