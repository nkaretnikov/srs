# Simple Review System

Review flashcards stored in a CSV file, separated by `;`:
```
question 0; answer 0
question 1; answer 1
...
```

Cards are shown in random order.  First the question is shown, then the user
can reveal the answer, self-assess the result and mark it accordingly.

When there are no cards left, the summary is printed: the correct to total ratio
and percentage.

`-s` can be used to swap questions and answers.  Press `q` or `esc` to quit.

Example (Japanese numbers):
```
./srs numbers.txt
9 (counting, months)
<Enter> to reveal answer

...

く (ku)
Correct? (y/n)

...

3
<Enter> to reveal answer

...

10/16 (62%)
```
