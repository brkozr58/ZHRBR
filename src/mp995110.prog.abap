*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9951                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP995100 MESSAGE-ID RP.

TABLES: P9951.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9951
                       DEFAULT P9951.

DATA: PSAVE LIKE P9951.
