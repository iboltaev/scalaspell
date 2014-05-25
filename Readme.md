Scalaspell

Scalaspell is simple spelling corrector server, written on scala.
Correction search algorithm is dictionary-based, with fast nearest search lookup.
It uses Netty to handle requests and scalatest for unit testing.
Results can be viewed in xml xml or json formats.

To start server, type "./run.sh" .
After this, server is available on 3535 port.

Parameters:
- "check" - a word to find corrections for
- "find" - search method. It can be k-nearest, which means "find me k nearest words to this" and passed as just number (find=5), or delta-nearest, which means "find me all words in distance smaller than d" and passed as d + number (find=d5)
- "format" - it's xml or json

Each found word is displayed by sequence of regular ("r") and correction ("c") tokens. Regular tokens displays matched part of original and corrected words, and correction token - corrected part respectively. If some letter sequence is present in original word, but isn't in corrected - it just drops. So, you can just concatenate all tokens and get corrected word.

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