%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sellaprime,
  [{description, "The Prime Number Shop"},
    {vsn, "4.0"},
    {modules, [sellaprime_app, sellaprime_supervisor, area_server,
      prime_server, load_balancer, prime_tester_server, lib_lin, lib_primes, my_alarm_handler]},
    {registered,[area_server, prime_server, sellaprime_super]},
    {applications, [kernel,stdlib]},
    {mod, {sellaprime_app,[]}},
    {start_phases, []}
  ]}.