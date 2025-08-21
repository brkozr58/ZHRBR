*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:04:11
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V006......................................*
TABLES: ZHRBR_V006, *ZHRBR_V006. "view work areas
CONTROLS: TCTRL_ZHRBR_V006
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V006. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V006.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V006_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V006.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V006_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V006_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V006.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V006_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T006                     .
