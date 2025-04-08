RCX02    TITLE '***** ICHRCX02 - RACINIT Post-processing exit ***** '
* ------------------------------------------------------------------ *
*                   I C H R C X 0 2                                  *
*                   ---------------                                  *
*        History : Original version.     Mar   19 2019 - LDB         *
*                  Updated with system                               *
*                  and sysplex name      April 06 2021 - LDB         *
*                                                                    *
* IBM Description: Post-processing exit for all RACINITs             *
*                                                                    *
*       Function : 1) Turn on Erase On Scratch if user has CONTROL   *
*                     access to a resource called                    *
*                     #EOS.sysplex.system.TEMPDSN                    *
*                     in the FACILITY class, where 'sysplex' is      *
*                     the sysplex name and 'system' is the system    *
*                     name.                                          *
*                                                                    *
*                : 2) Turn on Erase On Scratch if user has UPDATE    *
*                     access to the resource and also issue          *
*                     diagnostic messages.                           *
*                                                                    *
*                : 3) Issue diagnostics messages only if user has    *
*                     READ access to the resource.                   *
*                                                                    *
*   Return Codes : 0  Normal return - nothing to do                  *
*                  4  Retry                                          *
*                                                                    *
*           Note : If this code changes the value of the field       *
*                  pointed to by RCXRCODE or RCXREASN then the       *
*                  mainline RACHECK will use that code.              *
*                                                                    *
*   Maintenance  :                                                   *
*                                                                    *
* ------------------------------------------------------------------ *
ICHRCX02 AMODE 31
ICHRCX02 RMODE ANY
         EJECT
ICHRCX02 CSECT ,
         SAVE  (14,12),,ICHRCX02-&SYSDATE-&SYSTIME
*
         LR    R12,R15
         USING ICHRCX02,R12
*
         LR    R11,R1              Exit parameter list
         USING RCXPL,R11
*
         XR    R15,R15             Clear return code
*
         L     R1,RCXFLAG4         Point at flags 1
         TM    0(R1),RCXSTERA      ERASE status requested?
         BNO   MAIN090             No, get out quickly
*
         L     R1,RCXCOMP          Point at potential abend code
         CLC   0(R1),=F'0'         Abend code present?
         BNE   MAIN090             Yes, get out quickly
*
         L     R1,RCXRCODE         Point at return code
         L     R1,0(,R1)           Load actual return code
         C     R1,=F'0'            Is return code 0?
         BE    Main010             Yes, good
*
         C     R1,=F'4'            Is return code 4?
         BE    Main010             Yes, Good
*
         C     R1,=F'8'            Is return code 8?
         BNE   MAIN090             No, exit, not valid for reason 4
*
MAIN010  DS    0H
         L     R1,RCXFLAG4         Point at flags 3
         TM    0(R1),RCXSTERA      STATUS=ERASE requested?
         BNO   MAIN090             No, get out quickly
*
         L     R1,RCXFLAG3         Point at flags 3
         TM    0(R1),RCXDTYPT      This DSTYPE=T
         BO    MAIN090             Yes, get out quickly
*
         L     R1,RCXCLASS         Point at resource class code
         CLI   0(R1),X'07'         Could this be DATASET?
         BNE   MAIN090             No, get out quickly
*
         CLI   1(R1),C'D'          Could this be DATASET?
         BNE   MAIN090             No, get out quickly
*
         CLC   2(6,R1),=C'ATASET'  Is this DATASET?
         BNE   MAIN090             No, get out quickly
*
         LA    R2,WKend-WKstart    Length of Work Area
         XR    R10,R10             Subpool 0 (252 really)
*
         STORAGE OBTAIN,LENGTH=(R2),SP=(R10),LOC=ANY
*
         USING #WORK,R1
*
         ST    R2,WKlen            Store Length
         ST    R10,WKspl           Store Subpool
*
         ST    R1,8(,R13)          Link save areas
         ST    R13,4(,R1)              for z/OS
         LR    R13,R1              Set R13 for called routines
*
         DROP  R1                  #WORK
         USING #WORK,R13
*
MAIN050  DS    0H
         MVI   WkFlag1,X'00'       Clear flags
         XR    R15,R15             Clear return code, (no retry)
*
         BAS   R14,EOS0000         Do Erase-On-Scratch processing
*
         MVC   WKphase,=CL4'EXIT'
         LR    R9,R15
*
         L     R2,WKlen            Length of work area
         L     R10,WKspl           Subpool of work area
         LR    R1,R13              Point at workarea
*
         L     R13,4(,R1)          Rescue pointer to high save area
*
         PUSH  USING               Save addressability for subroutines
*
         DROP  R13
*
         STORAGE RELEASE,LENGTH=(R2),SP=(R10),ADDR=(1)
*
         LR    R15,R9              Reinstate return code
*
MAIN090  DS    0H
         RETURN (14,12),RC=(15)
*
         POP   USING               Reset addressability for Subroutines
*
         TITLE '***** ICHRCX02 - EOS Processing *****'
EOS0000  DS    0H
* ------------------------------------------------------------------- *
*                                                                     *
*        EOS0000 - Perform Erase-ON-Scratch processing                *
*                                                                     *
* ------------------------------------------------------------------- *
         STM   R0,R15,WK1save      Save mainline regs
         MVC   WKphase,=CL4'EOSC'
*
         L     R1,RCXFLAG3         Point to flags
         TM    0(R1),RCXPRPRA      Profile address specified?
         BO    EOS0100             Yes, branch
*
         L     R1,RCXENORP         Point to Entity name
         MVC   WKDSN,0(R1)         Save dataset name
         B     EOS0200
*
EOS0100  DS    0H
         Using RRPF,R1             Map resident profile
*
         MVC   WKDSN,DSPDSNM       Save dataset name
*
         DROP  R1                  RRPF
*
EOS0200  DS    0H
*
* ------------------------------------------------- *
*        TEMPORARY BYPASS - ALL DATASETS            *
* ------------------------------------------------- *
*                                                   *
*        B     EOS0600                              *
*                                                   *
* ------------------------------------------------- *
*
*        Is this a temporary data set?
         CLC   WKDSN(3),=C'SYS'    Start SYS?
         BNE   EOS0900
*
         LA    R2,WKDSN+3          Chars 4-8 Numeric?
         LA    R3,5
*
EOS0300  DS    0H
         CLI   0(R2),C'0'
         BL    EOS0900
*
         CLI   0(R2),C'9'
         BH    EOS0900
*
         LA    R2,1(,R2)
         BCT   R3,EOS0300
*
         CLC   WKDSN+8(2),=C'.T'   Chars 9-10
         BNE   EOS0900
*
         LA    R2,WKDSN+10         Chars 11-16 Numeric?
         LA    R3,6
*
EOS0400  DS    0H
         CLI   0(R2),C'0'
         BL    EOS0900
*
         CLI   0(R2),C'9'
         BH    EOS0900
*
         LA    R2,1(,R2)
         BCT   R3,EOS0400
*
         CLC   WKDSN+16(7),=C'.RA000.'    Third qualifier = RA000?
         BNE   EOS0900
*
*        Past this point we proceed as if this is a temporary dataset.
*
EOS0500  DS    0H
*
*        1. Construct RACF resource name
*
         USING PSA,R0              PSA addressability
*
         L     R4,FLCCVT           GET CVT address
*
         DROP  R0                  PSA
         USING CVTMAP,R4           CVT addressability
*
         MVC   WkSysnam,CVTSNAME   Save system name
         L     R5,CVTECVT          GET ECVT address
*
         DROP  R4                  CVT
         USING ECVT,R5             ECVT addressability
*
         MVC   WkSysplx,ECVTSPLX   Save sysplex name
*
         DROP  R5                  ECVT
*
*                                  #EOS.12345678.12345678.TEMPDSN
         MVC   WkEOSN(L'WkEOSN),=C'#EOS.                         '
         LA    R3,WkEOSN+5         Point at scan point
         MVC   0(8,R3),WkSysplx    Insert SYSPLEX name
         LA    R2,8                Get a counter
         XR    R4,R4               Zero a counter
*
EOS0510  DS    0H
         CLI   0(R3),C' '          End of string?
         BE    EOS0520             Yes, end loop
*
         LA    R3,1(,R3)           Bump address
         LA    R4,1(,R4)           Increment counter
         BCT   R2,EOS0510          Decrement and loop back
*
EOS0520  DS    0H
         MVI   0(R3),C'.'          Insert a dot
         LA    R3,1(,R3)           Bump address
         MVC   0(8,R3),WkSysnam    Insert SYSTEM name
*
         LA    R2,8                Get a counter
*
EOS0530  DS    0H
         CLI   0(R3),C' '          End of string?
         BE    EOS0540             Yes, end loop
*
         LA    R3,1(,R3)           Bump address
         LA    R4,1(,R4)           Increment counter
         BCT   R2,EOS0530          Decrement and loop back
*
EOS0540  DS    0H
         MVC   0(8,R3),=C'.TEMPDSN'  Complete resource name
*
         LA    R4,14(,R4)          R4 has length resource
         STH   R4,WkEOS            Store
         STH   R4,WkEOS+2          Store again
*
EOS0600  DS    0H
         MVC   WkRacrt(Racroutl),Racrout Format RACROUTE parmlist
         XC    WkSAFwrk(256),WkSAFwrk         Clear 256 bytes
         XC    WkSAFwrk+256(256),WkSAFwrk+256 Clear 256 bytes
*
         L     R5,RCXACEE          Point at ACEE address
         L     R5,0(,R5)           Point at actual ACEE
*
         USING SAFP,WkRacrt
*
         RACROUTE REQUEST=AUTH,                                        *
               MF=(E,WkRacrt),                                         *
               ENTITYX=(WKEOS,PRIVATE),                                *
               CLASS=FACILITY,                                         *
               LOG=NONE,                                               *
               STATUS=ACCESS,                                          *
               ATTR=READ,                                              *
               WORKA=WkSAFwrk,                                         *
               ACEE=(R5),                                              *
               RELEASE=2.1
*
         STM   R15,R1,WkRacrgs     Store return, reason and prof addr
*
         LTR   R15,R15             Test return code
         BNZ   EOS0800             Lower than READ, nothing to do
*
         L     R8,SAFPRRET         Load RACF return code
         C     R8,=F'20'           Access levels valid in reason code?
         BNE   EOS0800             No, nothing to do
*
         L     R7,SAFPRREA         Load RACF reason code
         C     R7,=F'0'            No access to resource?
         BE    EOS0800             Yes, simply exit
*
         C     R7,=F'12'           CONTROL access (or higher)?
         BNL   EOS0700             Yes, set EOS flag
*
*        Access is lower than CONTROL - Issue 5 or 6 messages
*
*        Format message RCX0201I
*
EOS0610  DS    0H
         MVC   WkText1(Text1L),Text1 Format msg
         MVC   Wkmsg1ds,WkDSN      Move in data set name
         MVC   WKWTO(WTOL),WTO     Format WTO
*
         WTO   TEXT=WkText1,MF=(E,WKWTO)
*
*        Format message RCX0202I
*
EOS0620  DS    0H
         MVC   WkText2(Text2L),Text2 Format msg
*
         L     R1,RCXRCODE         Point at existing RETCODE value
         MVC   WkRetcde,0(R1)      save reason code
         LA    R1,TRTAB            Point to table
         LA    R4,Wkmsg2rc         Point to target
         LA    R5,L'WkRetcde       Length source
         LA    R6,WkRetcde         Point to source
         TROT  R4,R6,1             Translate
*
         L     R1,RCXREASN         Point at existing REASON value
         MVC   WkReason,0(R1)      save reason code
         LA    R1,TRTAB            Point to table
         LA    R4,Wkmsg2rs         Point to target
         LA    R5,L'WkReason       Length source
         LA    R6,WkReason         Point to source
         TROT  R4,R6,1             Translate
*
         MVC   WKWTO(WTOL),WTO     Format WTO
*
         WTO   TEXT=WkText2,MF=(E,WKWTO)  Issue REASON message
*
*        Format message RCX0203I
*
EOS0630  DS    0H
         MVC   WkText3(Text3L),Text3 Format msg
*
         MVC   WkRescNm(L'WkEOSN),WkEOSN    Move resource name
*
         Using RRPF,R6             Map resident profile
*
*        Work out if profile exists. Remember result.
*
         ICM   R6,B'1111',WkPrfAdr Point at returned profile
         BZ    EOS0635             Nothing there, avoid msg
*
         CLC   SAFPRRET,=F'4'      Return code 4?
         BE    EOS0635             Yes, nothing to do
*
         OI    WkFlag1,WkProfEx    Show profile exists
         MVI   Wk3Comma,C','       Insert comma
         MVC   WkProfNm,DSPDSNM    Move profile name
*
         DROP  R6                  Resident profile
*
EOS0635  DS    0H
         MVC   WKWTO(WTOL),WTO     Format WTO
         WTO   TEXT=WkText3,MF=(E,WKWTO)    Issue codes message
*
*
*        Format message RCX0204I
*
EOS0640  DS    0H
         MVC   WkText4(Text4L),Text4 Format msg
         ST    R9,WkSAFrc          save SAF return code
         LA    R1,TRTAB            Point to table
         LA    R4,Wkmsg4sc         Point to target
         LA    R5,L'WkSAFrc        Length source
         LA    R6,WkSAFrc          Point to source
         TROT  R4,R6,1             Translate
*
         MVC   WkWork,SAFPRRET     save RACF return code
         LA    R1,TRTAB            Point to table
         LA    R4,Wkmsg4rc         Point to target
         LA    R5,L'WkWork         Length source
         LA    R6,WkWork           Point to source
         TROT  R4,R6,1             Translate
*
         MVC   WkWork,SAFPRREA     save RACF reason code
         LA    R1,TRTAB            Point to table
         LA    R4,Wkmsg4rs         Point to target
         LA    R5,L'WkWork         Length source
         LA    R6,WkWork           Point to source
         TROT  R4,R6,1             Translate
*
         MVC   WKWTO(WTOL),WTO     Format WTO
         WTO   TEXT=WkText4,MF=(E,WKWTO)    Issue codes message
*
         C     R7,=F'4'            Only READ access?
         BE    EOS0800             Yes, bypass EOS setting
*
*        Format message RCX0205I
*
EOS0650  DS    0H
         MVC   WKWTO(WTOL),WTO     Format WTO
         WTO   TEXT=Text5,MF=(E,WKWTO)
*
EOS0700  DS    0H
         L     R1,RCXREASN         Point to Reason code field
         MVC   0(4,R1),=F'4'       set return code for for EOS
*
EOS0800  DS    0H                  *** Free profile area ***
*
*        Free profile if needed
*
         TM    WkFlag1,WkProfEx    Does profile exist?
         BZ    EOS0900             No, avoid free
*
         L     R1,WkPrfAdr         Point at profile
*
         USING RRPF,R1             Resident profile mapping
*
         XR    R15,R15             Clear
         IC    R15,RRPSP           Get subpool
         XR    R0,R0               Clear
         ICM   R0,B'0111',RRPLEN   Get length
*
         STORAGE RELEASE,SP=(15),LENGTH=(0),ADDR=(1)
*
EOS0900  DS    0H                  End of processing
         LM    R0,R15,WK1save      Restore mainline regs
         BR    R14                 Return to mainline
*
         DROP  R11                 RCXPL
*
         LTORG
*
FACILITY DC    AL1(8),CL8'FACILITY'
*
         DS    0H
Text1    DC    AL2(Msgid1L)
Msgid1   DC    C'RCX02001I DSN='
MsgDSN   DC    CL44' '
Msgid1L  EQU   *-Msgid1
Text1L   EQU   *-Text1
*
         DS    0H
Text2    DC    AL2(Msgid2L)
Msgid2   DC    C'RCX02002I Current: RACFrc='
Msgretcd DC    CL8' '
         DC    C', RACFrs='
Msgreasn DC    CL8' '
Msgid2L  EQU   *-Msgid2
Text2L   EQU   *-Text2
*
         DS    0H
Text3    DC    AL2(Msgid3L)
Msgid3   DC    C'RCX02003I '
M3RescNm DC    CL30' '
         DC    C' '
M3ProfNm DC    CL44' '
Msgid3L  EQU   *-Msgid3
Text3L   EQU   *-Text3
*
         DS    0H
Text4    DC    AL2(Msgid4L)
Msgid4   DC    C'RCX02004I SAFrc='
M4SAFrc  DC    CL8' '
         DC    C', RACFrc='
M4RACFrc DC    CL8' '
         DC    C', RACFrs='
M4RACFrs DC    CL8' '
Msgid4L  EQU   *-Msgid4
Text4L   EQU   *-Text4
*
         DS    0H
Text5    DC    AL2(Msgid5L)
Msgid5   DC    C'RCX02005I Erase-On-Scratch selected'
Msgid5L  EQU   *-Msgid5
Text5L   EQU   *-Text5
*
WTO      WTO   TEXT=(*-*),MF=L,ROUTCDE=11
WTOL     EQU   *-WTO
*
         DS    0D
TRTAB    DC    CL32'000102030405060708090A0B0C0D0E0F'
         DC    CL32'101112131415161718191A1B1C1D1E1F'
         DC    CL32'202122232425262728292A2B2C2D2E2F'
         DC    CL32'303132333435363738393A3B3C3D3E3F'
         DC    CL32'404142434445464748494A4B4C4D4E4F'
         DC    CL32'505152535455565758595A5B5C5D5E5F'
         DC    CL32'606162636465666768696A6B6C6D6E6F'
         DC    CL32'707172737475767778797A7B7C7D7E7F'
         DC    CL32'808182838485868788898A8B8C8D8E8F'
         DC    CL32'909192939495969798999A9B9C9D9E9F'
         DC    CL32'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'
         DC    CL32'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'
         DC    CL32'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'
         DC    CL32'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'
         DC    CL32'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'
         DC    CL32'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'
*
Racrout  RACROUTE REQUEST=AUTH,                                        *
               ATTR=READ,                                              *
               CLASS=*-*,                                              *
               LOG=NONE,                                               *
               RELEASE=2.1,                                            *
               WORKA=*-*,                                              *
               MF=L
RacroutL EQU   *-Racrout
*
         TITLE '***** ICHRCX02 - EQUATES *****'
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         TITLE '***** ICHRCX02 - DATA AREAS *****'
PATCHES  DC    0A(PATCHES),40S(*)
*
LOCALMOD DC    CL8'*LENNIE'        LOCAL MODIFICATION NUMBER    F0RA039
         TITLE '***** ICHRCX02 - DSECTS *****'
*
#WORK    DSECT
WKstart  EQU   *
WKOSsave DS    18F
WKlen    DS    F
WKspl    DS    F
WK1save  DS    16F
WkPhase  DS    CL4
WkSAFrc  DS    F
WkWork   DS    F
WkDSN    DS    CL44
WkRacRgs DS    3F
         ORG   WkRacRgs
WkRetcde DS    F                   RACROUTE register 15
WkReason DS    F                   RACROUTE register 00
WkPrfAdr DS    F                   RACROUTE register 01
         ORG
WkFlag1  DS    X
WkProfEx EQU   B'10000000'         Show profile was returned
*
         DS    0F
WkSysnam DS    CL8
WkSysplx DS    CL8
WkEosRsc DS    CL44                Name of EOS profile used
*
WkEOS    DS    AL2(WkEOSL),AL2(WkEOSL)
WkEOSN   DS    C'#EOS.sysplex#.sysname#.TEMPDSN'
WkEOSL   EQU   *-WkEOSN
*
WkText1  DS    CL(Text1L)
         ORG   WkText1+2
Wkmsg1id DC    C'RCX02001I DSN='
Wkmsg1ds DC    CL44' '
*
         ORG
WkText2  DS    CL(Text2L)
         ORG   WkText2+2
Wkmsg2id DC    C'RCX02002I Current: RACFrc='
Wkmsg2rc DC    CL8' '
         DC    C', RACFrs='
Wkmsg2rs DC    CL8' '
*
         ORG
WkText3  DS    CL(Text3L)
         ORG   WkText3+2
Wkmsg3id DC    C'RCX02003I '
WkRescNm DC    CL30' '
Wk3Comma DC    C','
WkProfNm DC    CL44' '
*
         ORG
WkText4  DS    CL(Text4L)
         ORG   WkText4+2
Wkmsg4id DC    C'RCX02004I SAFrc='
Wkmsg4sc DC    CL8' '
         DC    C', RACFrc='
Wkmsg4rc DC    CL8' '
         DC    C', RACFrs='
Wkmsg4rs DC    CL8' '
*
         ORG
*
WkWTO    WTO   TEXT=(*-*),MF=L,ROUTCDE=11
*
WkRacrt  RACROUTE REQUEST=AUTH,                                        *
               ATTR=READ,                                              *
               LOG=NONE,                                               *
               RELEASE=2.1,                                            *
               WORKA=*-*,                                              *
               MF=L
*
         DS    0D
WkSAFwrk DS    2CL256
         DS    0D
WKend    EQU   *
*
         PRINT NOGEN
*
         CVT PREFIX=NO,DSECT=YES,LIST=NO CVT MAPPING
*
         IHAECVT DSECT=YES,LIST=NO ECVT MAPPING
*
         IHAPSA  ,                 PSA MAPPING
*
         ICHSAFP ,                 SAFP MAPPING
*
         ICHRCXP ,                 ICHRCX02 parm list
*
         ICHRRPF  ,                Resident profile map
*
         END   ,
