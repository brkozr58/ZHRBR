*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:02:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V003......................................*
TABLES: ZHRBR_V003, *ZHRBR_V003. "view work areas
CONTROLS: TCTRL_ZHRBR_V003
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V003. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V003.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V003_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V003_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V003_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V003_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T003                     .
