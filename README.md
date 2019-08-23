# Bookmarks Manager

A Haskell rewrite of [bookmarks.rb](https://github.com/TimWSpence/bookmarks.rb)

### Why a rewrite?

Because Haskell :) But also because building a binary with Stack is much
easier and more reliable than trying to set ruby up to `bundle exec` with the right Ruby/Gem
versions. My own installation of `bookmarks.rb` has broken on numerous occasions
when installing/changing system Ruby versions, etc

### Overview


I have yet to find a good bookmark syncing application or way of
managing my bookmarks so wrote this as an experiment.

The idea is to dump bookmarks into a YAML file of the format
```yaml
---

bookmarks:
- name: one
  url: http://foo.com
  tags:
  - one
  - example
- name: two
  url: https://bar.com
  tags:
  - two
  - example
```

and then have a tool which offers the following operations:
* list
* search
* add
* edit

The key advantages I see this offering are:
1. The data format is plain text so can be easily version controlled, edited, etc
2. The ability to attach metadata (tags) to enable more complex querying for
   bookmarks
3. Entirely browser-agnostic (although does require running a terminal as well
   to search for bookmarks but most terminal emulators will allow you to directly
   open links by clicking on them so I don't see this being prohibitively painful)

### Installation

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Ensure Stack's install directory is on your path (probably something like `$HOME/.local/bin`)
3. `stack install`
4. `bookmarks-exe --help`
