-define(SUCCESSFUL, {json, [{successful, true}]}).
-define(SUCCESSFUL_DATA(DataList), {json, [{successful, true}] ++ DataList}).
-define(ERROR(Errors), {json, [{successful, false}, {errors, Errors}]}).


