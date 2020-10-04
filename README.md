# PCParser

PCParser (Pseudo-Code Parser) was originally a standalone parser for the "dialect" of "pseudocode" used in
Rutgers University's Introduction to Computer Science course (01:198:111), but now contains an interpreter
implementation. Both components are written in F#.

## Building and Running

This project is written in F# for .NET Core 3.1. Install the .NET Core SDK [here](https://dotnet.microsoft.com/download).

### Building

```
$ dotnet restore
$ dotnet build
```

### Running

```
$ dotnet run --project src/Interpreter/ <path_to_pseudocode_file>
```

## What is a "dialect" of "pseudocode"? Why this project?

> In computer science, pseudocode is a plain language description of the steps in an algorithm or another system. Pseudocode often uses structural conventions of a normal programming language, but is intended for human reading rather than machine reading. It typically omits details that are essential for machine understanding of the algorithm, such as variable declarations and language-specific code. The programming language is augmented with natural language description details, where convenient, or with compact mathematical notation. The purpose of using pseudocode is that it is easier for people to understand than conventional programming language code, and that it is an efficient and environment-independent description of the key principles of an algorithm.

Given the above definition of pseudocode, it should logically follow that the notion of a pseudocode "dialect" doesn't
actually make a lot of sense. Pseudocode, in effect, should transcend the level of specificity that allows any particular
"dialect" to form or be documented. The benefits of using pseudocode are derived from its lack of specificity and its
closeness to human language. So what exactly is the "dialect" of pseudocode that this project revolves around?

The specification is [here](https://pdfhost.io/v/fwoMj~vWD_Rutgers_01198111_Pseudocode_Reference_Sheet_2020.pdf).

Having first-hand observed and helped numerous students get through the first
few weeks of class and seeing them have a significantly easier time with Java than Rutgers' pseudocode, I'm of the
opinion that the way in which this course employs what is otherwise a useful tool is ineffective at best, and
detrimental to learning at worst. Expecting students to conform to a strict syntax of "pseudocode" that they must
learn the boundaries of, as well as asking them to implement real algorithms completely free of practical context
in that syntax is, evidently, far from helpful to new programmers. And yet, it carries with it all the hardships
of learning an actual scripting language, e.g. Python, with none of the benefits. Students are asked to implement
algorithms to do relatively abstract or impractical things in what is effectively a real programming language without
ever having run a real program before.

This project is effectively satirical. By implementing a parser and interpreter that is, for the most part,
compliant with the pseudo-grammar linked above, we are able to weakly prove the assertion that Rutgers'
"pseudocode" leans much closer to an actual programming language than pseudocode.

## How does it Work?

PCParser uses FParsec, a parser combinator library, to parse pseudocode source-code into tokens which are
represented internally by algebraic data types. Each statement is parsed individually or as part of a larger
statement, and at the top level is stored in a linked list of statements. Each statement is mapped to an F#
function defined in the PCParser.Interpreter.Statements module that accepts and returns a `ProgramState`.
Each statement in the linked list is run through a recursive method wherein the resultant state after executing
one statement is passed to the next one.

## Known Problems

At this point, every statement that the linked specification specifies (of those which are actually applicable)
is recognised and executable by the interpreter. However, there are some significant issues that are yet to be resolved.

* Error messages
    - The interpreter can only give automatically generated parsing errors, and cannot yet give proper errors regarding
    issues in the execution itself. I will address the solution to this in a blog post.
* Conditions within Loops
    - There is a bug wherein conditionals within loops fail to be parsed. I am still investigating the cause of this
    issue.
* `Not` operator
    - The current operator implementation is dependent on operators being binary operators, however the `not` or `!`
    operator is a unary prefix operator, and so it's currently not able to be implemented. This can be fixed by changing
    the definition of the Operation parser type.