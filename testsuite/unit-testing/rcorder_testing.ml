(* rcorder_testing -- Test component bootstrap order

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken

module Application =
  Gasoline_Plain_Application

let component_list = [
  "motd", (["motd"; ], ["mountcritremote"; ]);
  "kerberos", (["kerberos"; ], ["NETWORKING"; ]);
  "sdpd", (["sdpd"; ], ["DAEMON"; ]);
  "auditd", (["auditd"; ], ["syslogd"; ]);
  "rtsold", (["rtsold"; ], ["netif"; ]);
  "rfcomm_pppd_server", (["rfcomm_pppd_server"; ], ["DAEMON";  "sdpd"; ]);
  "rwho", (["rwho"; ], ["DAEMON"; ]);
  "faith", (["faith"; ], ["netif"; ]);
  "cron", (["cron"; ], ["LOGIN";  "FILESYSTEMS"; ]);
  "ipsec", (["ipsec"; ], ["FILESYSTEMS"; ]);
  "static_ndp", (["static_ndp"; ], ["netif"; ]);
  "othermta", (["mail"; ], ["LOGIN"; ]);
  "ypbind", (["ypbind"; ], ["ypserv"; ]);
  "bsnmpd", (["bsnmpd"; ], ["NETWORKING";  "syslogd"; ]);
  "ntpd", (["ntpd"; ], ["DAEMON";  "ntpdate";  "FILESYSTEMS";  "devfs"; ]);
  "mdconfig", (["mdconfig"; ], ["swap";  "root"; ]);
  "stf", (["stf"; ], ["netif"; ]);
  "mountlate", (["mountlate"; ], ["DAEMON"; ]);
  "wpa_supplicant", (["wpa_supplicant"; ], ["mountcritremote"; ]);
  "routing", (["routing"; ], ["faith";  "netif";  "ppp";  "stf"; ]);
  "pfsync", (["pfsync"; ], ["FILESYSTEMS";  "netif"; ]);
  "mroute6d", (["mroute6d"; ], ["netif";  "routing"; ]);
  "hcsecd", (["hcsecd"; ], ["DAEMON"; ]);
  "ccd", (["disks"; ], []);
  "netoptions", (["netoptions"; ], ["FILESYSTEMS"; ]);
  "atm3", (["atm3"; ], ["atm2"; ]);
  "amd", (["amd"; ], ["rpcbind";  "ypset";  "nfsclient";  "FILESYSTEMS";  "ldconfig"; ]);
  "watchdogd", (["watchdogd"; ], ["FILESYSTEMS";  "syslogd"; ]);
  "sppp", (["sppp"; ], ["root"; ]);
  "lpd", (["lpd"; ], ["DAEMON"; ]);
  "bootparams", (["bootparams"; ], ["rpcbind";  "DAEMON"; ]);
  "ddb", (["ddb"; ], ["dumpon"; ]);
  "bluetooth", (["bluetooth"; ], ["DAEMON"; ]);
  "abi", (["abi"; ], ["archdep"; ]);
  "local_unbound", (["local_unbound"; ], ["FILESYSTEMS";  "netif";  "resolv"; ]);
  "tmp", (["tmp"; ], ["mountcritremote"; ]);
  "power_profile", (["power_profile"; ], ["FILESYSTEMS";  "syslogd"; ]);
  "hostid_save", (["hostid_save"; ], ["root"; ]);
  "root", (["root"; ], ["fsck"; ]);
  "random", (["random"; ], ["initrandom";  "FILESYSTEMS"; ]);
  "quota", (["quota"; ], ["mountcritremote";  "ypset"; ]);
  "geli2", (["geli2"; ], ["FILESYSTEMS"; ]);
  "pppoed", (["pppoed"; ], ["NETWORKING"; ]);
  "kldxref", (["kldxref"; ], ["FILESYSTEMS"; ]);
  "cleartmp", (["cleartmp"; ], ["mountcritremote";  "tmp"; ]);
  "devd", (["devd"; ], ["netif"; ]);
  "nfsd", (["nfsd"; ], ["mountd";  "hostname";  "gssd";  "nfsuserd"; ]);
  "mdconfig2", (["mdconfig2"; ], ["mountcritremote"; ]);
  "bthidd", (["bthidd"; ], ["DAEMON";  "hcsecd"; ]);
  "iscsid", (["iscsid"; ], ["NETWORK"; ]);
  "LOGIN", (["LOGIN"; ], ["DAEMON";  "utx"; ]);
  "dmesg", (["dmesg"; ], ["mountcritremote";  "FILESYSTEMS"; ]);
  "hastd", (["hastd"; ], ["NETWORKING";  "syslogd"; ]);
  "gptboot", (["gptboot"; ], ["mountcritremote"; ]);
  "swaplate", (["swaplate"; ], ["mountlate"; ]);
  "FILESYSTEMS", (["FILESYSTEMS"; ], ["root";  "mountcritlocal";  "cleanvar";  "zfs"; ]);
  "mountcritlocal", (["mountcritlocal"; ], ["root";  "hostid_save";  "mdconfig"; ]);
  "pf", (["pf"; ], ["FILESYSTEMS";  "netif";  "pflog";  "pfsync"; ]);
  "kpasswdd", (["kpasswdd"; ], ["kadmin"; ]);
  "nfsclient", (["nfsclient"; ], ["NETWORKING";  "mountcritremote";  "rpcbind"; ]);
  "fsck", (["fsck"; ], ["swap"; ]);
  "sshd", (["sshd"; ], ["LOGIN";  "FILESYSTEMS"; ]);
  "SERVERS", (["SERVERS"; ], ["mountcritremote";  "abi";  "ldconfig";  "savecore";  "watchdogd"; ]);
  "rpcbind", (["rpcbind"; ], ["NETWORKING";  "ntpdate";  "syslogd"; ]);
  "virecover", (["virecover"; ], ["mountcritremote";  "ldconfig"; ]);
  "kld", (["kld"; ], ["kldxref"; ]);
  "mrouted", (["mrouted"; ], ["netif";  "routing";  "FILESYSTEMS"; ]);
  "ftp-proxy", (["ftp-proxy"; ], ["DAEMON";  "pf"; ]);
  "DAEMON", (["DAEMON"; ], ["NETWORKING";  "SERVERS"; ]);
  "hostapd", (["hostapd"; ], ["mountcritremote"; ]);
  "var", (["var"; ], ["mountcritlocal";  "zfs"; ]);
  "auditdistd", (["auditdistd"; ], ["auditd"; ]);
  "ctld", (["ctld"; ], ["FILESYSTEMS"; ]);
  "defaultroute", (["defaultroute"; ], ["devd";  "faith";  "netif";  "stf"; ]);
  "ppp", (["ppp"; ], ["netif"; ]);
  "pflog", (["pflog"; ], ["FILESYSTEMS";  "netif";  "FILESYSTEMS"; ]);
  "resolv", (["resolv"; ], ["netif";  "FILESYSTEMS"; ]);
  "zfs", (["zfs"; ], ["mountcritlocal"; ]);
  "netwait", (["netwait"; ], ["NETWORKING"; ]);
  "serial", (["serial"; ], ["root"; ]);
  "syscons", (["syscons"; ], ["LOGIN"; ]);
  "nscd", (["nscd"; ], ["DAEMON"; ]);
  "route6d", (["route6d"; ], ["netif";  "routing"; ]);
  "adjkerntz", (["adjkerntz"; ], ["FILESYSTEMS";  "postrandom"; ]);
  "initrandom", (["initrandom"; ], ["dumpon";  "ddb"; ]);
  "keyserv", (["keyserv"; ], ["ypset"; ]);
  "local", (["local"; ], ["DAEMON"; ]);
  "ubthidhci", (["ubthidhci"; ], ["DAEMON"; ]);
  "static_arp", (["static_arp"; ], ["netif"; ]);
  "accounting", (["accounting"; ], ["mountcritremote"; ]);
  "rctl", (["rctl"; ], []);
  "netif", (["netif"; ], ["atm1";  "FILESYSTEMS";  "serial";  "sppp";  "sysctl";  "ipfilter";  "ipfs"; ]);
  "moused", (["moused"; ], ["DAEMON";  "FILESYSTEMS"; ]);
  "bgfsck", (["bgfsck"; ], ["cron";  "devfs";  "syslogd"; ]);
  "yppasswdd", (["yppasswdd"; ], ["ypserv";  "ypset"; ]);
  "atm2", (["atm2"; ], ["atm1";  "netif"; ]);
  "devfs", (["devfs"; ], ["mountcritremote"; ]);
  "addswap", (["addswap"; ], ["FILESYSTEMS";  "kld"; ]);
  "utx", (["utx"; ], ["DAEMON";  "FILESYSTEMS"; ]);
  "mountd", (["mountd"; ], ["NETWORKING";  "rpcbind";  "quota"; ]);
  "ipfilter", (["ipfilter"; ], ["FILESYSTEMS"; ]);
  "nfscbd", (["nfscbd"; ], ["NETWORKING";  "nfsuserd"; ]);
  "pwcheck", (["pwcheck"; ], ["mountcritremote";  "syslogd"; ]);
  "dhclient", (["dhclient"; ], []);
  "zvol", (["zvol"; ], ["hostid"; ]);
  "sendmail", (["mail"; ], ["LOGIN";  "FILESYSTEMS"; ]);
  "ypserv", (["ypserv"; ], ["rpcbind"; ]);
  "ntpdate", (["ntpdate"; ], ["NETWORKING";  "syslogd"; ]);
  "ypxfrd", (["ypxfrd"; ], ["rpcbind";  "ypserv"; ]);
  "gssd", (["gssd"; ], ["root"; ]);
  "hostname", (["hostname"; ], ["FILESYSTEMS"; ]);
  "mixer", (["mixer"; ], ["FILESYSTEMS"; ]);
  "ipmon", (["ipmon"; ], ["FILESYSTEMS";  "hostname";  "sysctl";  "FILESYSTEMS";  "ipfilter"; ]);
  "ipxrouted", (["ipxrouted"; ], ["SERVERS"; ]);
  "nfsuserd", (["nfsuserd"; ], ["NETWORKING"; ]);
  "nsswitch", (["nsswitch"; ], ["root"; ]);
  "NETWORKING", (["NETWORKING";  "NETWORK"; ], ["netif";  "netoptions";  "routing";  "ppp";  "ipfw";  "stf";  "faith";
"defaultroute";  "routed";  "mrouted";  "route6d";  "mroute6d";  "resolv";  "bridge";
"static_arp";  "static_ndp";  "local_unbound"; ]);
  "ipfs", (["ipfs"; ], ["ipnat"; ]);
  "iscsictl", (["iscsictl"; ], ["NETWORK";  "iscsid"; ]);
  "dumpon", (["dumpon"; ], ["zvol"; ]);
  "ldconfig", (["ldconfig"; ], ["mountcritremote";  "FILESYSTEMS"; ]);
  "cleanvar", (["cleanvar"; ], ["var"; ]);
  "statd", (["statd"; ], ["nfsclient";  "nfsd";  "rpcbind"; ]);
  "syslogd", (["syslogd"; ], ["mountcritremote";  "FILESYSTEMS";  "newsyslog";  "netif"; ]);
  "ugidfw", (["ugidfw"; ], []);
  "securelevel", (["securelevel"; ], ["adjkerntz";  "ipfw";  "ipfilter";  "pf"; ]);
  "ypupdated", (["ypupdated"; ], ["rpcbind";  "ypserv"; ]);
  "ipfw", (["ipfw"; ], ["ppp"; ]);
  "hostid", (["hostid"; ], ["sysctl"; ]);
  "apm", (["apm"; ], ["DAEMON"; ]);
  "newsyslog", (["newsyslog"; ], ["FILESYSTEMS";  "mountcritremote"; ]);
  "lockd", (["lockd"; ], ["nfsclient";  "nfsd";  "rpcbind";  "statd"; ]);
  "savecore", (["savecore"; ], ["dumpon";  "ddb";  "syslogd"; ]);
  "archdep", (["archdep"; ], ["mountcritremote"; ]);
  "geli", (["disks"; ], ["initrandom"; ]);
  "natd", (["natd"; ], []);
  "rtadvd", (["rtadvd"; ], ["DAEMON"; ]);
  "atm1", (["atm1"; ], ["root"; ]);
  "sysctl", (["sysctl"; ], []);
  "ip6addrctl", (["ip6addrctl"; ], ["FILESYSTEMS"; ]);
  "kadmind", (["kadmin"; ], ["kerberos"; ]);
  "msgs", (["msgs"; ], ["LOGIN"; ]);
  "postrandom", (["postrandom"; ], ["initrandom";  "random";  "FILESYSTEMS"; ]);
  "mountcritremote", (["mountcritremote"; ], ["NETWORKING";  "FILESYSTEMS";  "ipsec";  "netwait"; ]);
  "kfd", (["kfd"; ], ["NETWORK"; ]);
  "swap", (["swap"; ], ["disks"; ]);
  "routed", (["routed"; ], ["netif";  "routing"; ]);
  "inetd", (["inetd"; ], ["DAEMON";  "LOGIN";  "FILESYSTEMS"; ]);
  "jail", (["jail"; ], ["LOGIN";  "FILESYSTEMS"; ]);
  "ipnat", (["ipnat"; ], ["ipfilter"; ]);
  "localpkg", (["localpkg"; ], ["abi"; ]);
  "nisdomain", (["nisdomain"; ], ["SERVERS";  "rpcbind"; ]);
  "powerd", (["powerd"; ], ["DAEMON"; ]);
  "rarpd", (["rarpd"; ], ["DAEMON";  "FILESYSTEMS"; ]);
  "gbde", (["disks"; ], []);
  "timed", (["timed"; ], ["DAEMON"; ]);
  "bridge", (["bridge"; ], ["netif";  "faith";  "ppp";  "stf"; ]);
  "ftpd", (["ftpd"; ], ["LOGIN";  "FILESYSTEMS"; ]);
  "ypset", (["ypset"; ], ["ypbind"; ]);
  "apmd", (["apmd"; ], ["DAEMON";  "apm"; ]);
]

(* The component list was generated with the following shell script:

select()
{
    find /etc/rc.d -type f
}

words()
{
    sed -n -e "
/$1:/"'{
 s/.*: //
 s/\([A-Za-z0-9_][A-Za-z0-9_]*\)/"\1"; /g
 p
}' "$2"
}

process()
{
    while read name; do
        provide=$(words 'PROVIDE' "$name")
        require=$(words 'REQUIRE' "$name")
        printf '  "%s", [%s], [%s];\n' "${name##*/}" "${provide}" "${require}"
    done
}

select | process

The script could be used to extract a refereshed list of dependencies. *)

let bootstrap =
  ref []

let register (name, (provide, require)) =
  Application.Component.make
    ~name
    ~require
    ~provide
    ~description:"This composant is synthetised for testing purposes"
    ~bootstrap:(fun _ -> bootstrap := name :: !bootstrap)
    ()
  |> ignore

let validate _ =
  let barrier n =
    List.fold_left
      (fun acc (node, (providelst, _)) ->
         if List.mem n providelst && n <> node then node :: acc else acc)
      []
      component_list
  in
  let loop satisfied node =
    let provide, require =
      try List.assoc node component_list
      with Not_found -> ([], [])
    in
    if List.for_all (fun x -> List.mem x satisfied) (require @ barrier node)
    then
      (List.rev_append (node :: provide) satisfied)
    else
      Printf.ksprintf failwith "Component '%s' has unmet dependencies." node
  in
  List.fold_left loop [] (List.rev !bootstrap)
  |> ignore

let () =
  List.iter register component_list;
  Application.run "test_rcorder"
    ""
    "Test the rcorder function in the appkit"
    validate
