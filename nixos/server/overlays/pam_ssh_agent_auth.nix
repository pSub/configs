# Temporary fix for issue https://github.com/NixOS/nixpkgs/issues/386392.
final:  prev :
{
   pam_ssh_agent_auth = prev.pam_ssh_agent_auth.overrideAttrs (old: {
    fixupPhase = ''
      patchelf --add-needed ${prev.libgcc}/lib/libgcc_s.so.1 $out/libexec/pam_ssh_agent_auth.so
    '';
  });
}