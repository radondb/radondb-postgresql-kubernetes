
 ![](docs/images/logo_radondb.png)

  [English](README.md) | 中文

## 什么是 RadonDB PostgreSQL

[RadonDB PostgreSQL](https://github.com/radondb/radondb-postgresql-kubernetes) 是基于 `PostgreSQL` 的开源、高可用、云原生集群解决方案。支持一主多从高可用架构，并具备故障自动转移、集群节点伸缩自由等特性。

RadonDB PostgreSQL 支持在 `Kubernetes` 和 `KubeSphere` 平台部署。

## 架构图

- 通过 [repmgr](https://repmgr.org/docs/current/) 实现故障自动恢复
- 通过 [Pgpool-II](https://www.pgpool.net/) 负载均衡读写流量
- 通过 `PostgreSQL Streaming Replication` 确保数据的强一致性

![](docs/images/radondb-postgresql_Architecture_1.png)

## 核心功能

- PostgreSQL 高可用性
    - 主从秒级切换
    - 异步或同步流复制模式自由切换
    - 故障自动转移
- 集群节点自由伸缩
- 只读流量负载均衡
- 参数自定义调优

## 快速入门

- [在 Kubernetes 上通过 Git 部署 RadonDB PostgreSQL 集群](docs/deploy_radondb_postgresql_on_kubernetes_git.md)

- [在 Kubernetes 上通过 Helm Repo 部署 RadonDB PostgreSQL 集群](docs/deploy_radondb_postgresql_on_kubernetes_repo.md)

## 协议

RadonDB PostgreSQL 基于 Apache 2.0 协议，详见 [LICENSE](./LICENSE)。

<p align="center">
<br/><br/>
如有任何关于 RadonDB PostgreSQL 的问题或建议，请在 GitHub 提交 Issue 反馈。
<br/>
</a>
</p>
