# classifiers



# Negation of a graph containing Accept / Reject

Reject is not necessary?

Complementarity is good for negation, but does not persist through intersection / path enumeration.

NFAs are smaller than DFAs and we like that.

Using a Reject state for explicitly negated components of a set lets us have node state carried forward in an NFA through these operations (provenance at a minimum).

## Definitions

A NFA describes a set.

Items accepted by the NFA are definitionally within the set.

Items rejected by the NFA are definitionally excluded from the set.

? This is important to be able to construct the union of a negation with a

? Specifically to allow sets to be described in terms of items they do not contain.

## Negation through complementarity

Lets say I have a set defined as "(all items which match) a*"

this is:

```dot
    label = "a*";
    enter1 -> n1 [label = "a";];
    n1 -> n2 [label = "*";];
    n2 -> n2 [label = "*";];
    n2 [shape = "doublecircle";];
```

Where n2 is Accepting.

Lets say I have a set defined as "(all items which do not match a*)"

This is the negation of the above graph, regardless of how that negation is achieved.

Lets say I have a set defined as "a* "



## Work

