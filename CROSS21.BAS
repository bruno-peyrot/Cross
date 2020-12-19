
        REM ************************
        REM ***                  ***
        REM ***    CROSS v2.1    ***
        REM ***     (c) 2012     ***
        REM ***   Peyrot Bruno   ***
        REM ***                  ***
        REM ************************
        

        REM Initialisations

        DIM a$(8), f$(9)
        DIM arrive(200, 6)
        DIM tableau$(600, 5)
        DIM NoteClasse(4, 6, 2)
        DIM NbClasses(4)
        DIM NbEleves(4, 6, 2)
        DIM nbArrive(6)

        DATA "SixiŠme Filles","SixiŠme Gar‡ons","CinquiŠme Filles"
        DATA "CinquiŠme Gar‡ons","QuatriŠme Filles","QuatriŠme Gar‡ons"
        DATA "TroisiŠme Filles","TroisiŠme Gar‡ons"

        DATA "6F.CSV","6G.CSV","5F.CSV","5G.CSV","4F.CSV","4G.CSV"
        DATA "3F.CSV","3G.CSV"

        DATA 5,4,5,5

        FOR i = 1 TO 8: READ a$(i): NEXT i
        FOR i = 1 TO 8: READ f$(i): NEXT i
        FOR i = 1 TO 4: READ NbClasses(i): NEXT i
       
        bip$ = CHR$(7)


        REM Importation du fichier ‚lŠves

        OPEN "ELEVES.CSV" FOR INPUT AS #1
        i = 1
        WHILE NOT EOF(1)
                FOR j = 1 TO 5
                        INPUT #1, tableau$(i, j)
                NEXT j
               
                PRINT tableau$(i, 1); "-"; tableau$(i, 2); "-"; tableau$(i, 3); "-"; tableau$(i, 4); "-"; tableau$(i, 5)

                niveau = VAL(LEFT$(tableau$(i, 3), 1)) - 2
                division = ASC(RIGHT$(tableau$(i, 3), 1)) - 64
                IF UCASE$(tableau$(i, 4)) = "M" THEN sexe = 1 ELSE sexe = 2
                NbEleves(niveau, division, sexe) = NbEleves(niveau, division, sexe) + 1
                i = i + 1
        WEND
        nbt = i - 1
        CLOSE #1


        REM Boucle principale

        COLOR 7, 1, 1
10      CLS
        PRINT TAB(10); "**************************"
        PRINT TAB(10); "***   MENU PRINCIPAL   ***"
        PRINT TAB(10); "**************************"
        PRINT
        PRINT "  1: Saisie des r‚sultats d'une course"
        PRINT "  2: Edition des r‚sultats du cross"
        PRINT "  3: Quitter"
        PRINT
        INPUT "    Votre choix "; choix
        ON choix GOSUB 10000, 20000, 30000
        GOTO 10


        REM Saisie des r‚sultats d'une course

10000   CLS
        PRINT TAB(10); "*********************************************"
        PRINT TAB(10); "***   SAISIE DES RESULTATS D'UNE COURSE   ***"
        PRINT TAB(10); "*********************************************"
        PRINT
        FOR i = 1 TO 4
                PRINT "  "; CHR$(48 + i); ": "; a$(i)
        NEXT i
        PRINT "  5: QuatriŠme / TroisiŠme Filles"
        PRINT "  6: QuatriŠme / TroisiŠme Gar‡ons"
        PRINT "  7: Retour au menu principal"
        PRINT
        INPUT "    Votre choix "; choix
        IF choix < 1 OR choix > 7 THEN GOTO 10000
        IF choix = 7 THEN RETURN
        IF choix = 1 OR choix = 3 OR choix = 5 THEN sexe = 2 ELSE sexe = 1
        CLS
        course$ = a$(choix)
        IF choix = 5 THEN course$ = "QuatriŠme/TroisiŠme Filles"
        IF choix = 6 THEN course$ = "QuatriŠme/TroisiŠme Gar‡ons"
        PRINT "Entrez les num‚ros des coureurs de la course "; course$
        PRINT "Pour chaque num‚ro saisie, l'ordinateur affiche le nom de l'‚lŠve"
        PRINT "… fin de v‚rification. Validez grƒce … la touche [Entr‚e]. En cas"
        PRINT "d'erreur, tapez 'N'."
        PRINT "Quand le dernier coureur a ‚t‚ entr‚, tapez 'FIN' pour valider la"
        PRINT "saisie. Les r‚sultats seront automatiquement sauvegard‚s."
        PRINT
        PRINT
        nbArrive(choix) = 0
        index = 1
10010   PRINT "Rang nø"; index; ":";
        INPUT numero$
        IF UCASE$(numero$) = "FIN" THEN GOTO 10030
        FOR i = 1 TO nbt
                IF tableau$(i, 5) = numero$ THEN GOTO 10020
        NEXT i
        PRINT bip$: COLOR 12: PRINT "Ce num‚ro n'existe pas": COLOR 7: PRINT : GOTO 10010
10020   IF (sexe = 1 AND UCASE$(tableau$(i, 4)) = "F") OR (sexe = 2 AND UCASE$(tableau$(i, 4)) = "M") THEN PRINT bip$: COLOR 12: PRINT "Le sexe ne correspond pas": COLOR 7: PRINT : GOTO 10010
        FOR j = 1 TO index - 1
                IF tableau$(arrive(j, choix), 5) = numero$ THEN PRINT bip$: COLOR 12: PRINT "ElŠve d‚j… arriv‚": COLOR 7: PRINT : GOTO 10010
        NEXT j
        niveau = VAL(LEFT$(tableau$(i, 3), 1)) - 2
        IF niveau = 4 AND choix <> 1 AND choix <> 2 THEN PRINT bip$: COLOR 12: PRINT "Le niveau ne correspond pas": COLOR 7: PRINT : GOTO 10010
        IF niveau = 3 AND choix <> 3 AND choix <> 4 THEN PRINT bip$: COLOR 12: PRINT "Le niveau ne correspond pas": COLOR 7: PRINT : GOTO 10010
        IF (niveau = 1 OR niveau = 2) AND choix <> 5 AND choix <> 6 THEN PRINT bip$: COLOR 12: PRINT "Le niveau ne correspond pas": COLOR 7: PRINT : GOTO 10010
        PRINT tableau$(i, 1); " "; tableau$(i, 2); ": Confirmation ";
        INPUT reponse$
        IF UCASE$(reponse$) <> "O" AND reponse$ <> "" THEN GOTO 10010
        arrive(index, choix) = i
        nbArrive(choix) = nbArrive(choix) + 1
        index = index + 1
        GOTO 10010
10030   IF choix = 5 OR choix = 6 THEN GOTO 10040
        OPEN f$(choix) FOR OUTPUT AS #1
        FOR i = 1 TO nbArrive(choix)
                PRINT #1, CHR$(34) + tableau$(arrive(i, choix), 5) + CHR$(34) + ",";
                PRINT #1, CHR$(34) + tableau$(arrive(i, choix), 1) + CHR$(34) + ",";
                PRINT #1, CHR$(34) + tableau$(arrive(i, choix), 2) + CHR$(34) + ",";
                PRINT #1, CHR$(34) + tableau$(arrive(i, choix), 3) + CHR$(34)
        NEXT i
        CLOSE #1
        RETURN
10040   f4$ = f$(7 - sexe): f3$ = f$(9 - sexe)
        OPEN f4$ FOR OUTPUT AS #4
        OPEN f3$ FOR OUTPUT AS #3
        FOR i = 1 TO nbArrive(choix)
                abc$ = CHR$(34) + tableau$(arrive(i, choix), 5) + CHR$(34) + ","
                cde$ = CHR$(34) + tableau$(arrive(i, choix), 1) + CHR$(34) + ","
                fgh$ = CHR$(34) + tableau$(arrive(i, choix), 2) + CHR$(34) + ","
                ijk$ = CHR$(34) + tableau$(arrive(i, choix), 3) + CHR$(34)
                n = VAL(LEFT$(tableau$(arrive(i, choix), 3), 1))
                PRINT #n, abc$; cde$; fgh$; ijk$
        NEXT i
        CLOSE #3
        CLOSE #4
        RETURN


        REM Edition des r‚sultats du cross
       
20000   CLS
        PRINT TAB(10); "******************************************"
        PRINT TAB(10); "***   EDITION DES RESULTATS DU CROSS   ***"
        PRINT TAB(10); "******************************************"
        PRINT
        FOR i = 1 TO 8
                PRINT "  "; CHR$(48 + i); ": R‚sultats de la course "; a$(i)
        NEXT i
        PRINT "  9: Classement des SixiŠmes"
        PRINT "  10: Classement des CinquiŠmes"
        PRINT "  11: Classement des QuatriŠmes"
        PRINT "  12: Classement des TroisiŠmes"
        PRINT "  13: Retour au menu principal"
        PRINT
        INPUT "    Votre choix "; choix
        IF choix = 13 THEN RETURN
        IF choix >= 1 AND choix < 9 THEN GOTO 20010
        IF choix >= 9 AND choix < 13 THEN GOTO 20020
        GOTO 20000
20010   CLS
        PRINT "R‚sultats de la course "; a$(choix); " :"
        PRINT
        OPEN f$(choix) FOR INPUT AS #1
        WHILE NOT EOF(1)
                INPUT #1, numero$, nom$, prenom$, classe$
                PRINT numero$, nom$, prenom$, classe$
        WEND
        CLOSE #1
        PRINT
        PRINT "Veuillez presser une touche..."
        WHILE INKEY$ <> "": WEND
        WHILE INKEY$ = "": WEND
        GOTO 20000
20020   CLS
        PRINT "Classement des ";
        IF choix = 9 THEN
                PRINT "SixiŠmes :"
        ELSEIF choix = 10 THEN
                PRINT "CinquiŠmes :"
        ELSEIF choix = 11 THEN
                PRINT "QuatriŠmes :"
        ELSE
                PRINT "TroisiŠmes :"
        END IF
        PRINT
        FOR sexe = 1 TO 2
                Note = 0
                niveau = 13 - choix
                FOR i = 1 TO NbClasses(niveau)
                        NoteClasse(niveau, i, sexe) = 0
                        Note = Note + NbEleves(niveau, i, sexe)
                NEXT i
                OPEN f$(11 - 2 * niveau - sexe) FOR INPUT AS #1
                WHILE NOT EOF(1)
                        INPUT #1, numero$, nom$, prenom$, classe$
                        division = ASC(RIGHT$(classe$, 1)) - 64
                        NoteClasse(niveau, division, sexe) = NoteClasse(niveau, division, sexe) + Note
                        Note = Note - 1
                WEND
                CLOSE #1
                FOR i = 1 TO NbClasses(niveau)
                        NoteClasse(niveau, i, sexe) = NoteClasse(niveau, i, sexe) / NbEleves(niveau, i, sexe)
                NEXT i
        NEXT sexe
        FOR i = 1 TO NbClasses(niveau)
                tabl(i) = (NbEleves(niveau, i, 1) * NoteClasse(niveau, i, 1) + NbEleves(niveau, i, 2) * NoteClasse(niveau, i, 2)) / (NbEleves(niveau, i, 1) + NbEleves(niveau, i, 2))
                rang(i) = i
        NEXT i
20030   permut = 0
        FOR i = NbClasses(niveau) TO 2 STEP -1
                IF tabl(i) > tabl(i - 1) THEN
                        temp = tabl(i): tabl(i) = tabl(i - 1): tabl(i - 1) = temp
                        temp = rang(i): rang(i) = rang(i - 1): rang(i - 1) = temp
                        permut = 1
                END IF
        NEXT i
        IF permut <> 0 THEN GOTO 20030
        FOR i = 1 TO NbClasses(niveau)
                PRINT "Rang nø"; i; " : "; CHR$(50 + niveau); "E "; CHR$(64 + rang(i))
        NEXT i
        PRINT
        PRINT "Veuillez presser une touche..."
        WHILE INKEY$ <> "": WEND
        WHILE INKEY$ = "": WEND
        GOTO 20000


30000   END

