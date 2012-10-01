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

Road map
========

v0.1 DONE
Single project, build once, show build results

v0.2 DONE
Multiple projects, multiple builds, proper JS model

v0.3 DONE
Build queueing

v0.4 DONE
Re-building, build results history

v0.5 DONE
Hooks, build timeouts

v0.6
* DONE [ci skip] support,
* DONE project git change polling
* user authentication

v0.7
* branch list page, 
* DONE Build list paging
* DONE email notifications

v0.8
* basic .travis.yml support,
* remote builders, 
* IRC notifications

v0.9
* line-by-line build log updates
* Realtime data push

v1.0
Stable release

v1.1
Support for multiple Erlang versions via kerl

v1.2 DONE
GitHub webhook support DONE

v1.3
SVN/CVS support

v1.4
Build sandboxing using chroot

v1.5
Replace distributed Erlang with some MQ protocol

v2.0
Vagrant-based sandboxing


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
