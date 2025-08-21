*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:03:20
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V004......................................*
TABLES: ZHRBR_V004, *ZHRBR_V004. "view work areas
CONTROLS: TCTRL_ZHRBR_V004
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V004. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V004.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V004_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V004_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V004_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V004_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T004                     .
