Scalaspell

Scalaspell is simple spelling corrector server, written on scala.
Correction search algorithm is dictionary-based, with fast nearest search lookup.
It uses Netty to handle requests and scalatest for unit testing.
Results can be viewed in xml xml or json formats.

To start server, type "./run.sh" .
After this, server is available on 3535 port.

Parameters:
- "check" - a word to find corrections for
- "find" - search method. It can k-nearest, which means "find me k nearest words to this" and passed as just number (find=5), or delta-nearest, which means "find me all words in distance smaller than d" and passed as d + number (find=d5)
- "format" - it's xml or json

Example: 

http://localhost:3535/?check=amnesia&find=d2&format=xml

will produce

```xml
<ws>
  <w>
    <r>amnesia</r>
  </w>
  <w>
    <r>amnesia</r>
    <c>c</c>
  </w>
  <w>
    <r>amnesi</r>
    <c>c</c>
  </w>
</ws>
```