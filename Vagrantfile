Vagrant.configure(2) do |config|
  config.vm.box = 'ubuntu/trusty64'
  config.vm.provider :virtualbox do |vbox|
    vbox.memory = 1024
  end
  config.vm.provision :shell, inline: <<-SHELL
    apt-get -y autoremove
    apt-get -y update
    apt-get -y install haskell-platform
  SHELL
end
