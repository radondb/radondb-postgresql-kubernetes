
 ![](docs/images/logo_radondb.png)
 
 English | [中文](README_zh.md) 

## What is RadonDB PostgreSQL

[RadonDB PostgreSQL](https://github.com/radondb/radondb-postgresql-kubernetes) is an open-source, cloud-native, highly availability cluster solutions based on PostgreSQL. 

RadonDB PostgreSQL supports [Kubernetes](https://kubernetes.io) or [KubeShpere](https://kubesphere.com.cn) platforms.

## Architecture

- Supporting automatic failover through [repmgr](https://repmgr.org/docs/current/)
- Load-balancing I/O traffic through [Pgpool-II](https://www.pgpool.net/)
- Keeping data consistency through `PostgreSQL Streaming Replication`

![](docs/images/radondb-postgresql_Architecture_1.png)

## Features

- High Availability PostgreSQL database
    - Leader-follower switching in second-level
    - Asynchronous or Synchronous streaming replication
    - Automatic failover
- Cluster Management
- Failover and Load-Balancing
- Parameter Tuning

## Quick Start

- [Installing RadonDB PostgreSQL through Git on Kubernetes](docs/deploy_radondb_postgresql_on_kubernetes_git.md)

- [Installing RadonDB PostgreSQL through Helm Repo on Kubernetes](docs/deploy_radondb_postgresql_on_kubernetes_repo.md)

## License

RadonDB PostgreSQL is released under the Apache 2.0, see [LICENSE](./LICENSE).

Please submit any RadonDB PostgreSQL bugs, issues, and feature requests to GitHub Issue.
