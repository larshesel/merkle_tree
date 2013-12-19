# Merkel Trees

I have been fascinated with Merkle trees for a long time, and decided
I wanted to build my own in Erlang.

My long term goal (if I don't loose interest) is to build generic
Merkle trees for synchronizing many different kinds of data such as
large files, directories, filesystems or other kinds of data sets.

Think about it as a generic rsync framework. The entities doesn't need
to be files.

## Current features:

* Build a Merkle tree bottom up over a raw binary (in memory only)
* Build a Merkle tree using positional insert.
* Fetch and verify an entire Merkle tree (from an Erlang gen_server).

## Ideas:

* Refetch partial Merkle trees, fetching only the missing pieces.
* Implement a tcp protocol for synchronizing Merkle trees over the
  Internet.
* Build a Merkle tree over a directory and synchronize it somewhere
  (like rsync).

## Resources

You can read more about Merkle trees here:

* <http://en.wikipedia.org/wiki/Merkle_tree>
* <http://crypto.stackexchange.com/tags/hash-tree/info>
