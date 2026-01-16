       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-ACCOUNT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "D:\h\Prog-prob3\ACCFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD BANK-FILE.
       01 OUTREC                   PIC X(150).

       WORKING-STORAGE SECTION.
       01 INPUT-DATA.
           05 ACCNO-IN             PIC 9(10).
           05 ACCNAME-IN           PIC X(25).
           05 G-CODE-IN            PIC X.
           05 TRANSTYPE-IN         PIC X.
           05 AMT-IN               PIC 9(7)V99.
           05 ACCTYPE-IN           PIC X.
           05 INIDEP-IN            PIC 9(7)V99.
           05 BRAN-CD-IN           PIC X(3).
       01 COMPUTE-DATA.
           05 GNAME-IN             PIC X(6).
           05 TRANSNAME-IN         PIC X(12).
           05 ACCTYPENAME-IN       PIC X(15).
           05 BAL-IN               PIC 9(9)V99.
           05 BRAN-NAME-IN         PIC X(15).
           05 ERR-MSG              PIC X(50).
       01 CHECKER.
           05 EOFSW                PIC X.
           05 FLAGSW               PIC X VALUE 'N'.
       01 H1.
           05 FILLER               PIC X(32) VALUE SPACES.
           05 FILLER               PIC X(16) VALUE "China Trust Bank".
           05 FILLER               PIC X(32) VALUE SPACES.
       01 H2.
           05 FILLER               PIC X(35) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE "Makati City".
           05 FILLER               PIC X(34) VALUE SPACES.
       01 H3.
           05 FILLER               PIC X(31) VALUE SPACES.
           05 FILLER               PIC X(18) VALUE "Customer's Account".
           05 FILLER               PIC X(31) VALUE SPACES.
       01 SH1.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE "Account".
           05 FILLER               PIC X(9) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE "Account". 
           05 FILLER               PIC X(9) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE "Transaction".
           05 FILLER               PIC X(9) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE "Account". 
           05 FILLER               PIC X(10) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE "Balance". 
           05 FILLER               PIC X(2) VALUE SPACES.
       01 SH2.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE "No".
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE "Name". 
           05 FILLER               PIC X(13) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE "Name".
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE "Type Name". 
           05 FILLER               PIC X(19) VALUE SPACES.
       01 REC-OUT.
           05 ACCNO-OUT            PIC 9(10).
           05 FILLER               PIC X(1) VALUE SPACE.
           05 ACCNAME-OUT          PIC X(25).
           05 FILLER               PIC X(1) VALUE SPACE.
           05 TRANSNAME-OUT        PIC X(12).
           05 FILLER               PIC X(1) VALUE SPACE.
           05 ACCTYPENAME-OUT      PIC X(15).
           05 FILLER               PIC X(1) VALUE SPACE.
           05 BAL-OUT              PIC ZZZ,ZZZ,ZZ9.99.
       
       SCREEN SECTION.
       01 HEADER.
           05 BLANK SCREEN.
           05 LINE 1 COL 33 VALUE "China Trust Bank".
           05 LINE 2 COL 36 VALUE "Makati City".
           05 LINE 3 COL 32 VALUE "Customer's Account".
       01 LAYOUT.
           05 LINE 5 COL 1 VALUE "Account Number: ".
           05 LINE 6 COL 1 VALUE "Account Name: ".
           05 LINE 7 COL 1 VALUE "Gender Code: ".
           05 LINE 8 COL 1 VALUE "Gender Name: ".
           05 LINE 9 COL 1 VALUE "Transaction Type: ".
           05 LINE 10 COL 1 VALUE "Transaction Name: ".
           05 LINE 11 COL 1 VALUE "Amount: ".
           05 LINE 12 COL 1 VALUE "Account Type: ".
           05 LINE 13 COL 1 VALUE "Account Type Name: ".
           05 LINE 14 COL 1 VALUE "Initial Deposit: ".
           05 LINE 15 COL 1 VALUE "Balance: ".
           05 LINE 16 COL 1 VALUE "Branch Code: ".
           05 LINE 17 COL 1 VALUE "Branch Name: ".
       01 SCR-PR1.
           05 LINE 5 COL 35 PIC 9(10) USING ACCNO-IN REQUIRED AUTO.
           05 LINE 6 COL 35 PIC X(25) USING ACCNAME-IN REQUIRED AUTO.
       01 SCR-G-CODE.
           05 LINE 7 COL 35 PIC X USING G-CODE-IN REQUIRED AUTO.
       01 SCR-GEN-NAME.
           05 LINE 8 COL 35 PIC X(6) USING GNAME-IN HIGHLIGHT.
       01 SCR-PR2.
           05 LINE 9 COL 35 PIC X USING TRANSTYPE-IN REQUIRED AUTO.
       01 SCR-TRANS-NAME.
           05 LINE 10 COL 35 PIC X(12) USING TRANSNAME-IN HIGHLIGHT.
       01 SCR-PR3.
           05 LINE 11 COL 35 PIC 9(7).99 USING AMT-IN REQUIRED AUTO.
       01 SCR-ACC-TYPE.
           05 LINE 12 COL 35 PIC X USING ACCTYPE-IN REQUIRED AUTO.
       01 SCR-ACC-TYPENAME.
           05 LINE 13 COL 35 PIC X(15) USING ACCTYPENAME-IN HIGHLIGHT.
       01 SCR-PR4.
           05 LINE 14 COL 35 PIC 9(7).99 USING INIDEP-IN REQUIRED AUTO.
       01 SCR-BAL.
           05 LINE 15 COL 35 PIC 9(9).99 USING BAL-IN HIGHLIGHT.
       01 SCR-PR5.
           05 LINE 16 COL 35 PIC X(3) USING BRAN-CD-IN REQUIRED AUTO.
       01 SCR-BR-NAME.
           05 LINE 17 COL 35 PIC X(15) USING BRAN-NAME-IN REQUIRED AUTO.
       01 SCR-CONTINUE.
           05 LINE 19 COL 1 VALUE "Input Another Record (Y/N)?: ".
           05 LINE 19 COL 47 PIC X USING EOFSW REQUIRED AUTO.
       01 SCR-ERROR.
           05 LINE 19 COL 1 FROM ERR-MSG.
       
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM HEADERS-RTN

           PERFORM PROCESS-RTN UNTIL EOFSW = 'N' OR 'n'

           PERFORM FINISH-RTN
           STOP RUN.
       
       HEADERS-RTN.
           OPEN OUTPUT BANK-FILE
           WRITE OUTREC FROM H1
           WRITE OUTREC FROM H2
           WRITE OUTREC FROM H3
           WRITE OUTREC FROM SPACES 
           WRITE OUTREC FROM SH1
           WRITE OUTREC FROM SH2.
       
       PROCESS-RTN.
           DISPLAY HEADER
           DISPLAY LAYOUT
           
           INITIALIZE INPUT-DATA COMPUTE-DATA
           DISPLAY SCR-PR1
           ACCEPT SCR-PR1

           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-G-CODE
               ACCEPT SCR-G-CODE
               MOVE FUNCTION UPPER-CASE(G-CODE-IN) TO G-CODE-IN

               EVALUATE G-CODE-IN
                   WHEN 'F'
                       MOVE 'Female' TO GNAME-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'M'
                       MOVE 'Male' TO GNAME-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN OTHER 
                       MOVE 'ERROR: F or M only!' TO ERR-MSG
                       DISPLAY SCR-ERROR
               END-EVALUATE 
           END-PERFORM

           DISPLAY SCR-GEN-NAME

           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-PR2
               ACCEPT SCR-PR2
               MOVE FUNCTION UPPER-CASE(TRANSTYPE-IN) TO TRANSTYPE-IN

               EVALUATE TRANSTYPE-IN
               WHEN 'D'
                   MOVE 'Deposit' TO TRANSNAME-IN
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
                   MOVE 'Y' TO FLAGSW
               WHEN 'W'
                   MOVE 'Withdrawal' TO TRANSNAME-IN
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
                   MOVE 'Y' TO FLAGSW
               WHEN OTHER 
                   MOVE 'ERROR: W or D only!' TO ERR-MSG
                   DISPLAY SCR-ERROR
               END-EVALUATE 
           END-PERFORM 

           DISPLAY SCR-TRANS-NAME

           DISPLAY SCR-PR3
           ACCEPT SCR-PR3
           
           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-ACC-TYPE
               ACCEPT SCR-ACC-TYPE
               MOVE FUNCTION UPPER-CASE(ACCTYPE-IN) TO ACCTYPE-IN

               EVALUATE ACCTYPE-IN
               WHEN 'S'
                   MOVE 'Savings Deposit' TO ACCTYPENAME-IN
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
                   MOVE 'Y' TO FLAGSW
               WHEN 'C'
                   MOVE 'Cheking Account' TO ACCTYPENAME-IN
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
                   MOVE 'Y' TO FLAGSW
               WHEN 'D'
                   MOVE 'Dollar Account' TO ACCTYPENAME-IN
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
                   MOVE 'Y' TO FLAGSW
               WHEN OTHER 
                   MOVE 'ERROR: S, C, D only!' TO ERR-MSG
                   DISPLAY SCR-ERROR
               END-EVALUATE 
           END-PERFORM 

           DISPLAY SCR-ACC-TYPENAME
           
           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-PR4
               ACCEPT SCR-PR4

               EVALUATE TRANSTYPE-IN
                   WHEN 'D'
                       COMPUTE BAL-IN = INIDEP-IN + AMT-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'W'
                       IF AMT-IN > INIDEP-IN
                           MOVE 'ERROR: Insufficient Balance' TO ERR-MSG
                           DISPLAY SCR-ERROR
                       ELSE 
                           COMPUTE BAL-IN = INIDEP-IN - AMT-IN
                           MOVE SPACES TO ERR-MSG
                           DISPLAY SCR-ERROR
                           MOVE 'Y' TO FLAGSW
               END-EVALUATE 
           END-PERFORM 

           DISPLAY SCR-BAL
           
           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-PR5
               ACCEPT SCR-PR5
               MOVE FUNCTION UPPER-CASE(BRAN-CD-IN) TO BRAN-CD-IN

               EVALUATE BRAN-CD-IN
                   WHEN 'PAR'
                       MOVE 'Paranaque' TO BRAN-NAME-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'PAS'
                       MOVE 'Pasay' TO BRAN-NAME-IN 
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'MAN'
                       MOVE 'Mandaluyong' TO BRAN-NAME-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'SME'
                       MOVE 'Sta. Mesa' TO BRAN-NAME-IN   
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN 'SJA'
                       MOVE 'San Juan' TO BRAN-NAME-IN
                       MOVE SPACES TO ERR-MSG
                       DISPLAY SCR-ERROR
                       MOVE 'Y' TO FLAGSW
                   WHEN OTHER 
                       MOVE 'ERROR: PAR, PAS, MAN, SME, SJA Only!'
                           TO ERR-MSG
                       DISPLAY SCR-ERROR
               END-EVALUATE 
           END-PERFORM 

           DISPLAY SCR-BR-NAME

           MOVE ACCNO-IN TO ACCNO-OUT
           MOVE ACCNAME-IN TO ACCNAME-OUT
           MOVE TRANSNAME-IN TO TRANSNAME-OUT
           MOVE ACCTYPENAME-IN TO ACCTYPENAME-OUT
           MOVE BAL-IN TO BAL-OUT
           WRITE OUTREC FROM REC-OUT

           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-CONTINUE
               ACCEPT SCR-CONTINUE
               MOVE FUNCTION UPPER-CASE(EOFSW) TO EOFSW

               IF EOFSW = 'Y' OR 'N'
                   MOVE 'Y' TO FLAGSW
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
               ELSE 
                   MOVE "ERROR: Y or N Only" TO ERR-MSG
                   DISPLAY SCR-ERROR
               END-IF 
           END-PERFORM.

       FINISH-RTN.
           CLOSE BANK-FILE.

                                   


