*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9950                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM mp995000 MESSAGE-ID rp.

TABLES: p9950.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <pnnnn> STRUCTURE p9950
                       DEFAULT p9950.

DATA: BEGIN OF bdcdata OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: psave LIKE p9950.

DATA: t003  TYPE TABLE OF zhrbr_t003.
DATA: BEGIN OF t9951 OCCURS 0 .
DATA: field .
        INCLUDE TYPE pa9951     .
DATA: END OF t9951.

DATA : s9951   TYPE pa9951.
DATA : ok9951  TYPE pa9951.

DATA : s591s   TYPE t591s.

DATA : v_icraa TYPE zhrbr_de029.
DATA : v_ibann TYPE zhrbr_de025.
DATA : v_kalan TYPE zhrbr_de012."Kalan Borç
DATA : v_kesil TYPE zhrbr_de012."Kesilen Tutar

DATA : v_new   TYPE c.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABS'
CONSTANTS: BEGIN OF c_tabs,
             tab1 LIKE sy-ucomm VALUE 'TABS_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABS_FC2',
           END OF c_tabs.

*&SPWIZARD: DATA FOR TABSTRIP 'TABS'
CONTROLS:  tabs TYPE TABSTRIP.
DATA:      BEGIN OF g_tabs,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'MP995000',
             pressed_tab LIKE sy-ucomm VALUE c_tabs-tab1,
           END OF g_tabs.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TCONT' ITSELF
CONTROLS: tcont TYPE TABLEVIEW USING SCREEN 2200.

DATA:ok_code LIKE sy-ucomm.
