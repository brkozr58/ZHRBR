
PROCESS BEFORE OUTPUT.
*         general infotype-independent operations
  MODULE before_output.
  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE p9951.

  MODULE hidden_data.
*
PROCESS AFTER INPUT.
*---------------------------------------------------------------------*
*  process exit commands
*---------------------------------------------------------------------*
  MODULE exit AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*         processing after input
*---------------------------------------------------------------------*
*
*         check and mark if there was any input: all fields that
*         accept input HAVE TO BE listed here
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9951-begda.
    FIELD p9951-endda.
    FIELD p9951-manue.
    FIELD p9951-acikl.
    FIELD p9951-odmtr.
    FIELD p9951-icrid.
    MODULE input_status ON CHAIN-REQUEST.
  ENDCHAIN.
*---------------------------------------------------------------------*
*      process functioncodes before input-checks                      *
*---------------------------------------------------------------------*
  MODULE pre_input_checks.
*---------------------------------------------------------------------*
*         input-checks:                                               *
*---------------------------------------------------------------------*

*   insert check modules here:
  MODULE user_command.
*  ...

*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9951-begda.
    FIELD p9951-endda.
    FIELD rp50m-sprtx.
    FIELD p9951-manue.
    FIELD p9951-acikl.
    FIELD p9951-odmtr.
    FIELD p9951-icrid.
    MODULE post_input_checks.
  ENDCHAIN.
*


