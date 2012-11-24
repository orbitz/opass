# Opass
opass is a simple password database written in ocaml.  It uses gpg
for storing the db and all operation are performed in memory.  No tricks
are used to to ensure the security of this program.  It depends on being
run in a secure location, but since the data is stored on gpg it should
be safe to store in an insecure location.

Right now opass just does things I care about so don't expect it to work
well for you.

# Operations

In each operation a database is create if none exists

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
