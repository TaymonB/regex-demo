Vagrant.configure(2) do |config|
  config.vm.box = 'ubuntu/trusty64'
  config.vm.provision :shell, privileged: true, inline: <<-SHELL
    apt-get -y autoremove
    apt-get -y install haskell-platform
  SHELL
  config.vm.provision :shell, privileged: false, inline: <<-SHELL
    cabal install control-monad-omega
  SHELL
end
