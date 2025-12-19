*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRBR_V001......................................*
TABLES: ZHRBR_V001, *ZHRBR_V001. "view work areas
CONTROLS: TCTRL_ZHRBR_V001
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRBR_V001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRBR_V001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRBR_V001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRBR_V001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRBR_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRBR_V001_TOTAL.

*.........table declarations:.................................*
TABLES: ZHRBR_T001                     .
