PROCESS BEFORE OUTPUT.
*         general infotype-independent operations
  MODULE modify_subscreen.
*  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
*  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE p9950_2200.
*
  MODULE HIDDEN_DATA.
*
PROCESS AFTER INPUT.
*---------------------------------------------------------------------*
*  process exit commands
*---------------------------------------------------------------------*
  MODULE EXIT AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*         processing after input
*---------------------------------------------------------------------*
*
*         check and mark if there was any input: all fields that
*         accept input HAVE TO BE listed here
*---------------------------------------------------------------------*
  CHAIN.
*    FIELD P9951-BEGDA.
*    FIELD P9951-ENDDA.
    FIELD P9951-MANUE.
*    FIELD P9951-ACIKL.
    FIELD P9951-ODMTR.
    FIELD P9951-ICRID.
    MODULE input_status_subscreen ON CHAIN-REQUEST.
  ENDCHAIN.
*---------------------------------------------------------------------*
*      process functioncodes before input-checks                      *
*---------------------------------------------------------------------*
  MODULE PRE_INPUT_CHECKS.
*---------------------------------------------------------------------*
*         input-checks:                                               *
*---------------------------------------------------------------------*

*   insert check modules here:

*  ...

*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
*    FIELD P9951-BEGDA.
*    FIELD P9951-ENDDA.
*    FIELD RP50M-SPRTX.
    FIELD P9951-MANUE.
*    FIELD P9951-ACIKL.
    FIELD P9951-ODMTR.
    FIELD P9951-ICRID.
*    MODULE POST_INPUT_CHECKS.
  ENDCHAIN.
*


