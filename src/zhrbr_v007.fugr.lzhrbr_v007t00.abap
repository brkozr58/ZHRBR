*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.01.2020 at 21:04:45
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRBR_V007......................................*
TABLES: ZHRBR_V007, *ZHRBR_V007. "view work areas
CONTROLS: TCTRL_ZHRBR_V007
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V007. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V007.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V007_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V007.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V007_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V007_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V007.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V007_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T007                     .
