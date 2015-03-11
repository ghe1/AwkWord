# AwkWord: A word game

A simple and straightforward word game at [main.html]()

## Rules
Score the highest possible score you can by making words with the given letters in the time allotted.

Challenge your friends by making a common seed and seeing how far you can get from that seed. The letters are deterministically generated from each seed, but randomly generated if no seed is provided.

Letters are not reused once a valid word has been made with them so be strategic.

## Controls
### Game State
**Esc** will pause the game for you, as will clicking the **pause button**. You can end the game from there and restart.

### Choosing Letters
You can pick letters by either **clicking** on the letters you want to move, or by **typing** in the letters that you want.

**Backspace** functionality exists, but is tricky, since backspace is also mapped to "back" in most browsers. Not recommended to use it, but ⌘+delete will work for Mac if necessary.

**Enter** will send the string of letters you typed to be verified by the dictionary. If the string is a word, you'll get points, otherwise the letters will return to your hand.

### Your Letters
**Space** or the **shuffle button** will shuffle your letters for you, so you can get a new perspective on your hand.

If your hand is completely hopeless, you can click **ReRoll button** to generate a completely new set of letters. Be careful though, you lose points for rerolling.

## Ravi Mode
[mainRavi.html]() will direct you to the Ravi Mode version of the game. Self-explanatory.
