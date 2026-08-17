# A Calculator
[Demo Link](https://vigorous-wilson-c30c63.netlify.app/)
---
This calculator attempts to implement the [Shunting-yard](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) algorithm for processing an infix expression as a postfix one (Reverse Polish Notation).
Using this algorithm allows the calculator to be a bit more sophisicated when it comes to evaluating lengthier expressions with multple operators of varying precedence.
One of my primary goals while implementing the alogrithm was to take advantage of elm's type system as much as possible in an effort to effectively constrain the possible states.
This complicated the application code to a high degree, I have done my best to go through an leave meaningful comments.

### Getting Started
To get started you only need the following commands
```bash
$ npm install
$ npm start
```
`npm install` will install a local version of elm to your project, along with elm-live, which is a simple package for building and serving elm projects.
`npm start` will spin up the elm-live server, which will build the project and serve it at http://localhost:8000.

### Some Concessions
I made two concessions to get this project done.
  1. There is only one clear button that clears the entire working operation, and reinitializes the application back to an empty starting state. I was finding it very difficult to separate between the states created by the clear and all-clear buttons. At least within the context of my data model.
  2. There is no consideration for expressions that evaluate to a value with more digits than can be rendered. This is an enhancement I'd like to make, but I cut it for time.

### Pressing Equals After an Operator
Pressing `=` while an operator is still waiting for its right hand operand is ambiguous, so the calculator has two rules for it.

If there is nothing else to evaluate, the operator is applied to the working operand and itself. `3 + =` is 6, and `8 ÷ =` is 1.

If the expression already holds an operation, that dangling operator is dropped and the rest of the expression is evaluated. For example:

    1. Press 3
    2. Press +
    3. Press 4
    4. Press x
    5. Press 2
    6. Press ÷
    7. Press =
  Pressing `÷` collapses `4 x 2` and displays 8, leaving `3 + 8 ÷` pending. The `÷` never gets an operand, so it is discarded and the result is `3 + 8`, or 11. This matches the macOS calculator.


## Why Elm?
As I grow as a developer I find myself leaning towards a "functional first" style. 
Whenever possible and reasonable I aim for pure functions and I treat data as immutable. I find that this is often helpful in mitigating complexities and surprising state changes.
I wanted to put this style to the test by using a genuinely functional language, with a strong type system.
I love the idea of elm. It combines the excellent benefits of functional programming with singular method of managing application state. 
I have yet to use elm on a project of significant size or complexity, I saw this assignment as an opportunity to put it to the test.
