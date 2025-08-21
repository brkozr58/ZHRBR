FUNCTION ZHRBR_FG001_001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_CLASSNAME) TYPE  SBDST_CLASSNAME
*"     REFERENCE(IV_CLASSTYPE) TYPE  SBDST_CLASSTYPE
*"     REFERENCE(IV_OBJECT_KEY) TYPE  SBDST_OBJECT_KEY
*"  TABLES
*"      T_VALUE STRUCTURE  ZHRBR_S002
*"----------------------------------------------------------------------

  MOVE : iv_classname  TO gv-classname,
         iv_classtype  TO gv-classtype,
         iv_object_key TO gv-object_key.

  MOVE : t_value[] TO gt_value[].

  CALL SCREEN 0100.





ENDFUNCTION.
