# Opass
opass is a simple password database written in ocaml.  It uses gpg
for storing the db and all operation are performed in memory.  No tricks
are used to to ensure the security of this program.  It depends on being
run in a secure location, but since the data is stored on gpg it should
be safe to store in an insecure location.

Right now opass just does things I care about so don't expect it to work
well for you.

# Operations

In each operation a database is created if none exists

## Adding

    opass add

This adds an entry to the database. Duplicates are not allowed.
The database is copied to a .bak file prior to writing the contents
back out incase the write fails.

## Searching

    opass search <term>

Searches for a term.  The password or note's name, location, and username
are all searched case insenitively.  `opass search` will print the entire
db

## Editing

    opass edit <entry name>

Edits a single entry.  You can change all attributes about it, even
generating a new password.  The entry name must be the exact name.  Use
quotes if it contains spaces.

## Deleting

    opass del <entryname1> <entryname2> ...

Will confirm and delete all matching entry names.  The entry names must be exact
matches.  Use quotes if there are spaces in the name.

## Merging

    opass merge -t 1password -f /path/to/text/dump

This merges a 1Password exported file into opass.  1Password may have duplicates
and this will simply error in that case, more helpful debugging will be added
in the future.

## Generating random passwords

    opass password [-l length] [-c charset]

opass can be used to generate random passwords.  Valid value for charset are
`any`, `alpha`, `alphanum`.  Example:

    opass password -l 12

# Neat! But...

## Why should I use this over 1Password?

Maybe you shouldn't!  I wrote this because I wanted a solution with a CLI interface
and worked on Linux.  I think 1Password is a great product and I even paid for a
license, but I have come to value portability and control over my platform more.

## Why should I use this at all?

Maybe you shouldn't!  I wrote this for my own needs and it develops at that pace.
I am willing to handle feature requests as long as they are reasonable and correspond
to my own goals, but if you think the product is incomplete at this point and aren't
comfortable enough in Ocaml to fork it yourself then you probably shouldn't use opass.
