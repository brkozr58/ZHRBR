*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:03:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V005......................................*
TABLES: ZHRBR_V005, *ZHRBR_V005. "view work areas
CONTROLS: TCTRL_ZHRBR_V005
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V005. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V005.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V005_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V005.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V005_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V005_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V005.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V005_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T005                     .
