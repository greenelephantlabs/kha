kha
===

Lightweight Erlang continious integration server.

Goals:

* Lightweight
* Easy to setup
* Easy to use
* Extensible

Planned features:

* Support for rebar
* Git support
  * per branch builds
 * Git config setup
* Support for unixy build scripts
* Plugins support
  * Notifications
  * Hooks - pre-build, post-build, etc.
* One file deployment
* Web UI for reports

Notifications:
* Jabber
* Email
* other via plugins

Sandboxing
==========

Kha supports sandboxing via "lxc" containers. Tested on Ubuntu
12.10. Requires a kernel at least 3.5.0-21. For "lxc" to work, Kha has
to be run under a user with password-less sudo permissions.

Road map
========

v0.1 DONE
* Single project, build once, show build results

v0.2 DONE
* Multiple projects, multiple builds, proper JS model

v0.3 DONE
* Build queueing

v0.4 DONE
* Re-building, build results history

v0.5 DONE
* Hooks, build timeouts

v0.6 DONE
* [ci skip] support,
* project git change polling
* user authentication

v0.7 DONE
* Build list paging 
* email notifications
* Jabber/XMPP notifications

v0.8
* line-by-line build log updates [DONE]
* basic .travis.yml support, [DONE]
* build sandboxing using lxc [DONE]

v0.9
* support for multiple Erlang versions via kerl
* realtime data push
* remote builders

v1.0
* use user/project convention
* fully working UI
* stable release

v1.1
* GitHub webhook support [DONE]
* branch list page

v1.2
* complex notification options
* IRC notifications

v1.3
* SVN/CVS support

v1.4
* Support foreign architectures using Vagrant/KVM

v1.5
* Replace distributed Erlang with some MQ protocol


v1.0 architecture
==================

Rough sketch of v1.0 architecture:

1. Angular.js on client side
2. SockJS for communication, hence allowing to connect to any remote server
3. Server side
   * Control node
   * Builder node

Control node:
* mnesia for storing projects and builds
* project as a process

Project:
* name
* load config from .travis.yml
* build in place OR
* git clone url
* build instructions (manual or from git/config)

Builder:
* queue manager
* builder as a process

Communications:
* Both project and builder processes will be broadcasting updates with
gproc:bcast/3
* Session process will subscribe to appropriate keys

Notifications
* Notificators will be independent processes which will be getting
broadcasts from builders/processes
