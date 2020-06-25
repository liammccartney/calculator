# A Calculator
[Demo Link](https://vigorous-wilson-c30c63.netlify.app/)
---
This calculator attempts to implement the [Shunting-yard](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) algorithm for processing an infix expression as a postfix one (Reverse Polish Notation).
Using this algorithm allows the calculator to be a bit more sophisicated when it comes to evaluating lengthier expressions with multple operators of varying precedence.
One of my primary goals while implementing the alogrithm was to take advantage of elm's type system as much as possible in an effort to effectively constrain the possible states.
This complicated the application code to a high degree, I have done my best to go through an leave meaningful comments.

### Some Concessions
I made two concessions to get this project done.
  1. There is only one clear button that clears the entire working operation, and reinitializes the application back to an empty starting state. I was finding it very difficult to separate between the states created by the clear and all-clear buttons. At least within the context of my data model.
  2. There is no consideration for expressions that evaluate to a value with more digits than can be rendered. This is an enhancement I'd like to make, but I cut it for time.
  3. There is one set of operations that do not correctly evaluate. I let this go because I see it as an unlikely scenario, as well as I don't fully understand yet what's happening. Here is an example of what I mean:

    1. Press 3
    2. Press +
    3. Press 4
    4. Press x
    5. Press 2
    6. Press ÷
    7. Press =
  On the macOS calculator this will evaluate to 8.375 and is expressed as "3 / 4 * 2 + 8".
  I was unable to find a way to get my data model to work in this scenario. Instead it falls back to the last operand of a valid expression, in this case 4.


## Why Elm?
As I grow as a developer I find myself leaning towards a "functional first" style. 
Whenever possible and reasonable I aim for pure functions and I treat data as immutable. I find that this is often helpful in mitigating complexities and surprising state changes.
I wanted to put this style to the test by using a genuinely functional language, with a strong type system.
I love the idea of elm. It combines the excellent benefits of functional programming with singular method of managing application state. 
I have yet to use elm on a project of significant size or complexity, I saw this assignment as an opportunity to put it to the test.
