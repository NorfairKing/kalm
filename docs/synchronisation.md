---
title: Synchronisation
---


Kalm synchronises emails between an IMAP server and its local database.
This is the technical documentation for how that works.

### UIDs

[The IMAP protocol](https://datatracker.ietf.org/doc/html/rfc3501) specifies [Unique identifier (UID)](https://datatracker.ietf.org/doc/html/rfc3501#section-2.3.1.1) and `UIDValidity` message attributes.

> The unique identifier of a message MUST NOT change during the session, and SHOULD NOT change between sessions. 
> Any change of unique identifiers between sessions MUST be detectable using the UIDVALIDITY mechanism discussed below.

> The combination of mailbox name, UIDVALIDITY, and UID must refer to a single immutable message on that server forever.
> This does not include message numbers, nor does it include attributes that can be set by a STORE command (e.g., FLAGS).


Conclusion:

* A triple `(MailboxName, UIDValidity, UID)` can specify an immutable message.
* A message can correspond to multiple of such triples.



### Synchronisation

TODO how do we actually sync?

