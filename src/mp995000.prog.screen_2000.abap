
PROCESS BEFORE OUTPUT.
  MODULE before_output.
  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE p9950.

*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TABS'
  MODULE tabs_active_tab_set.
  CALL SUBSCREEN tabs_sca
    INCLUDING g_tabs-prog g_tabs-subscreen.
*         general infotype-independent operations

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
    FIELD p9950-begda.
    FIELD p9950-endda.
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

*  ...
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TABS'
  CALL SUBSCREEN tabs_sca.
  MODULE tabs_active_tab_get.
*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9950-begda.
    FIELD p9950-endda.
    FIELD rp50m-sprtx.
    MODULE post_input_checks.
  ENDCHAIN.
*
