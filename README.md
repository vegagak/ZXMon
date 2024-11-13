# ZXMon is a hexadecimal code monitor utility for the Sinclair ZX81 home computer

Written in Z80 machine language in 1983. Presented here in assembly language

* It is a load-and-stay-resident program. Programs can be loaded and saved while ZXMon is loaded above RAMTOP
* You can use it to view memory
* Can also type in new hex digits to change memory
* Programs can be edited and then ZXMon exited
* Jump to any memory address address

I built it to enter machine code into already existing long REM statements


![Display address](./images/ZXMon_b.png?raw=true)

# System Requirements
* Requires 16K RAM minimum
* Please use in SLOW mode

# Loading and running
1. Memory must be reserved for ZXMon. This is done by changing the system variable "RAMTOP"
   first enter:
   
	`POKE 16389,124`
 
   then  enter:
   
	`NEW`

   then `LOAD` ZXMon and `RUN`
   
   This should be done when first turning on the machine -- to avoid having to load ZXMon twice.

3. To use ZXMon at any time, enter:

	`LET L= USR 31802`

   -Even after `NEW` -- or after loading another program-

![Jump to address](./images/ZXMon_a.png?raw=true)

# USAGE
```
SPACE          EXIT THE MONITOR
ARROW KEYS     MOVE THE CURSOR
0-9, A-F       CHANGE THE MEMORY
SHIFT-D        ENTER DISPLAY ADDRESS
SHIFT-J        ENTER JUMP ADDRESS
SHIFT-S        ENTER BLOCK START ADDRESS
SHIFT-E        ENTER BLOCK END ADDRESS
LEFT BRACKET   PAGE DISPLAY UP
RIGHT BRACKET  PAGE DISPLAY DOWN

(BETWEEN START AND END)
   EDIT        INSERT BYTE (ZEROED),
               MOVE REST OF BLOCK UP, 
               LAST BYTE DELETED

   RUBOUT      DELETE CURRENT BYTE,
               MOVE REST OF BLOCK DOWN,
               ADD ZERO TO END OF BLOCK

TO             JUMP TO 'J' ADDRESS
```

Program listing as printed by the <a href='https://www.google.com/search?q=ZX81+printer&udm=2'>ZX Printer</a>
<br>![ZXMon program LIST on ZX Printer](./images/zxmon.JPG?raw=true)
