# Metacircular-Evaluator

This is an implementation of a Metacircular evaluator for Scheme heavily inspired by the evaluator described in Structure and Interpretation of Computer Programs (SICP).

This repository contains:

- Source Code: The implementation is provided in the ``metacircular-evaluator.scm`` file. It closely follows the evaluator described in Structure and Interpretation of Computer Programs (SICP), with some extensions, including the implementation of ``and``, ``or``, and ``let*`` as special forms.

- Documentation: The ``report.pdf`` file includes a detailed exploration of key concepts related to the evaluator. While it does not serve as an explanation of the code, it introduces and discusses fundamental ideas that motivate the study of the evaluator, as well as insights from Gerald Jay Sussman’s lectures and the SICP textbook.

-----

# How to Run the Code

To run the code provided in metacircular-evaluator.scm, follow these steps:

+ Install the ``DrRacket`` IDE, if not already installed.
+ Open ``DrRacket`` and go to the menu: ``File → Install Package``.
+ Search for and install the ``sicp`` language package.
+ Load the ``metacircular-evaluator.scm`` file into DrRacket and execute the code by pressing the ``Run`` button.

Instead following the last step, once ``DrRacket`` and ``sicp`` are installed, the code can be run by running

```bash 
racket metacircular-evaluator.scm
```

on your terminal.

The ``sicp`` language package adds necessary Scheme functionalities, specifically related to SICP (Structure and Interpretation of Computer Programs). Features like ``set-car!`` and ``set-cdr!``, which are used to modify pairs in the ``sicp`` language, require this package for full compatibility.

-----

# How to Run the Tests

To execute the test suite for the evaluator, follow these steps:

+ Run the code.
+ Type the command ``run-tests!!`` and press Enter.

Observe the output as the tests are executed. The results will indicate which tests passed or failed.

-----

# Bibliography

- Harold Abelson e Gerald Jay Sussman. Structure and Interpretation of Computer Programs. 2nd. Cambridge, MA: MIT Press, 1996. isbn: 978-0262510875. [Online Version](https://mitpress.mit.edu/sites/default/files/sicp/index.html).

- Gerald Jay Sussman. Lecture 7A: Metacircular Evaluator, Part 1. Accessed on 2024-12-24. [Watch on YouTube](https://youtu.be/aAlR3cezPJg).

- Gerald Jay Sussman. Lecture 7B: Metacircular Evaluator, Part 2. Accessed on 2024-12-24. [Watch on YouTube](https://youtu.be/QVEOq5k6Xi0).
