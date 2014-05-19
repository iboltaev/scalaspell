scalaspell
==========

Dictionary-based spelling correction server, written in scala with Netty.
Implements correction in terms of Levenstein metric.
Answers to requests of type check=word_to_correct&toFind=search_method&format=output_format,
where "search_method" is one of ({count of needed nearest corrections}, d{maximum difference between correction and original})
and "output_format" is one of (xml, json).

Example: check=amnesa&d=2&format=xml
Output: "
<ws>
    <w>
        <r>amnes</r>
        <c>i</c>
        <r>a</r>
    </w>
</ws>
"
, where "<r>" means "regular", "<c>" - "correction"

Build is (temporarly & poorly) implemented via make. To build, you should correct CLASSPATH make variable.
Just "make" command compiles application & run all unit tests.
"make SPEC" compiles application & runs specified SPEC unit tests.
"make run" - runs application on 3536 port.
