{application, tws, [
  {description, "IB TWS API client"},
  {vsn, "1.0.0"}
  {modules, [tws, tws_sup]},
  {registered, [tws]},
  {applicatins, [kernel, stdlib]},
  {mod, {tws_sup, []}},
]}.

%% vim: set et ts=2 sw=2 ai invlist si cul nu:
