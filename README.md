# Lexer

## The first stage
- Due to the difficulty of working directly with regular expressions to check whether a
word belongs to a language, real lexers go through several intermediate stages before
starting to analyze the text. These stages build a DFA based on the regular expression.


- Stage 1 consists of:

converting a regular expression to an NFA (using the Thompson Algorithm)

converting an NFA to a DFA (using the Subset Construction Algorithm).

### The Prenex Form of Regular Expressions

The Prenex Form is inspired by the Polish notation of arithmetic expressions 
(in which expressions like 1 + 2 * 3 are written as + 1 * 2 3). The advantage of this
notation (and of the Prenex form in our case) is that parentheses are no longer 
necessary to express any expression. For example, (1 + 2) * 3 is written in Polish 
notation as * + 1 2 3.

Regular expressions in Prenex form are composed of:

(1) atoms

(2) the names of operations (UNION, STAR, CONCAT, PLUS, MAYBE) directly followed 
by other sub-expressions.

An atom can be:

- An alphanumeric character (e.g. 0 or a)
- Any character enclosed in single quotes (e.g. 'a' or ';')
- One of the keywords eps (for the empty string) or void (for the empty language).


The operations:

- PLUS, denoted in standard notation as e+, represents the regex e followed by 
the Kleene star operator (*).
- MAYBE, denoted in standard notation as e?, represents the regex e union with the
empty string (ϵ), i.e. e ∪ ϵ.
- The remaining operations have their standard meanings.

### Implementation
The implementation consists of parsing the Prenex expression (using an internal 
tree structure known as an Abstract Syntax Tree or AST), and then converting it to an
NFA and DFA using the algorithms mentioned earlier.


## The second stage

Step 2 consists of parsing a regular expression (regex) written in the conventional
way and converting it to Prenex form.

<p>
    In this stage I had to implement the part of transforming an expression from normal regex form to prenex form that my NFA and DFA can process.
</p>

---

<p>
    To implement this part I split this transformation into smaller processing parts:
</p>

    1. Eliminated syntactic sugars such as: [a-z] or [0-9]
    2. Added a concatenation symbol where it was necessary. I used '_' as the concatenation symbol, 
        it was added in places like: "abc" -> "a_b_c"
    3. Using two stacks I transform the formatted regex into a prenex
    4. I write the regex special symbols as prenex operators.

<p>
    Flow example: 
    (ab(b|c)*)* => (a_b_(b|c)*)* => *__ab*|bc => STAR CONCAT CONCAT a b STAR UNION b c
</p>
