@echo off
IF %1.==. GOTO No1
IF %2.==. GOTO No2
IF %3.==. GOTO No3
IF %4.==. GOTO No4

nmap -Pn --host-timeout 201 --max-retries 0 -p %1 %4
timeout 1
nmap -Pn --host-timeout 201 --max-retries 0 -p %2 %4
timeout 1
nmap -Pn --host-timeout 201 --max-retries 0 -p %3 %4
timeout 1
ssh %4

GOTO End1

:No1
  ECHO No param 1
GOTO End1

:No2
  ECHO No param 2
GOTO End1

:No3
  ECHO No param 3
GOTO End1

:No4
  ECHO No param 4
GOTO End1

:End1
