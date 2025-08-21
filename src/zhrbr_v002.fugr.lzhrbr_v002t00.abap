*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:02:20
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V002......................................*
TABLES: ZHRBR_V002, *ZHRBR_V002. "view work areas
CONTROLS: TCTRL_ZHRBR_V002
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V002_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T002                     .
