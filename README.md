# **Connect-Four-AI-BOT-Player**

# **README for Prolog Connect 4 Project**

## **Project Overview**
This project implements the classic Connect 4 (Puissance 4) game in Prolog with an AI opponent. The game features:

- A 7x6 game board (7 columns, 6 rows)
- Human vs. computer gameplay
- Three different AI implementations with varying difficulty levels
- Win detection for horizontal, vertical, and diagonal connections

---

## **Features**

### **Game Modes**
- Human vs. Human
- Human vs. Computer
- Computer vs. Computer

### **AI Implementations**
- Random move selection
- Minimax algorithm with depth-limited search
- Position-based evaluation function

### **Game Mechanics**
- Proper token dropping simulation (gravity)
- Win detection for 4-in-a-row patterns
- Board visualization in the console

---

## **How to Run**

1. Ensure you have SWI-Prolog installed.
2. Load the game file in SWI-Prolog:
   ```prolog
   ['connect4.pl'].
   ```
3. Start the game:
   ```prolog
   run.
   ```

---

## **Game Setup**

The game can be configured by modifying these predicates at the top of the file:

```prolog
player(1, computer).  % or 'human'
player(2, human).     % or 'computer'

player_mark(1, 'x').  % player 1 uses 'x'
player_mark(2, 'o').  % player 2 uses 'o'
```

---

## **AI Configuration**

The AI difficulty can be adjusted by modifying the minimax depth parameter (currently set to 3):

```prolog
minimax(D, B, M, S, U) :-
    D=3,  % Change this value to adjust difficulty
    utilitytest(B, U),
    !.
```

---

## **Game Controls**

During human turns, enter the column number (1-7) where you want to drop your token.

The game automatically detects wins and draws.

---

## **Project Structure**

Key predicates include:

- `winner/2` : Detects winning conditions
- `minimax/5` : Implements the AI decision-making
- `print_board/1` : Displays the current game state
- `move/4` : Handles player moves and board updates

---

## **Dependencies**

- SWI-Prolog (tested with version 8.2.4)

---

## **Team Members**

- BEL HADJ YOUSSEF Ahmed
- GUIMARAES Bernardo
- SAGUES Rafael
- CARVAJAL Mateo
- AVORNICITA Madalina

---

## **Acknowledgments**

This project was developed as part of the 4IF 2025 curriculum at INSA LYON, exploring declarative programming and game AI concepts in Prolog.

---
