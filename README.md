# network-remote

[![Build Status](https://www.travis-ci.com/MechDancer/network-remote.svg?branch=master)](https://www.travis-ci.com/MechDancer/network-remote)

MechDancer 远程接入协议库的另一个实现，可与 [JVM 实现](https://github.com/MechDancer/remote)相互通信。

## 结构

本库基于 [conduit](https://hackage.haskell.org/package/conduit)，需要导入相关模块：

```haskell
import Conduit
```
* `Network.Remote.Socket.MulticastSocket` 管理套接字资源。
* `Network.Remote.Resource.Address` 管理地址资源。
* `Network.Remote.Resource.Networks` 查找网卡。
*  `Network.Remote.Socket.Broadcaster` 处理广播。
*  `Network.Remote.Socket.Receiver` 处理接收。
* `Network.Remote.Protocol.Conduit.ByteString` 按封装好的协议处理数据 。

## 使用

需要先使用 `newManager` 创建组播套接字管理器。接收和广播是独立的两部分，对于接收，`ReceiverConfig` 是必要的，其包含了接收者的名字、地址资源，与套接字管理器；而对于发送，仅需发送者名字与套接字管理器。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent (forkIO, threadDelay)
import Network.Remote
import Network.Remote.Resource.Networks
import Network.Remote.Protocol.Conduit.ByteString

someFunc = do
  manager <- newManager "233.33.33.33" 23333
  addresses <- newAddresses
  networks <- scanNetwork
  print "Scanning network..."
  print networks
  let terminalName = "Alice"
      receiverConfig = ReceiverConfig "Bob" addresses networks manager
  withManager manager $ openSocket (head networks)
```

上面的测试代码会尝试打开操作系统网卡中的第一张，在实际使用中 *可能* 不需要手动打开网卡，因为接收者会监听来自组播地址中所有的数据报，并打开与数据来源相同子网地址的网卡。**打开不合适的网卡会导致程序直接去世。**

### 接收

Bob： 

```haskell
  -- Receiver "Bob"
  forkIO $ runConduit $
    receivePacket receiverConfig
      .| mapC (\RemotePacket {..} -> "[Bob] Receive packet from " ++ sender ++ ", with command " ++ show command ++ ", and payload: " ++ show payload)
      .| printC
```

`(receivePacket receiverConfig) :: (MonadIO m) => ConduitT i RemotePacket m () `，创建了数据为 `RemotePacket` 的上游，在运行该管道时会触发阻塞 IO。`RemotePacket` 包含发送者名字、命令、负载。除了使用 `receivePacket` 函数外，推荐直接使用 `receivePayload :: (MonadIO m, Command c) => ReceiverConfig -> [c] -> ConduitT i ByteString m ()`。它会使用命令集 `[c]` 来过滤使用者关心的远程包，并创建数据为 `ByteString` 的上游。

### 广播

Alice:

```haskell
  -- Sender "Alice"
  runConduit $
    yieldMany [1 .. 1000]
      .| mapMC (\x -> threadDelay 100 >> (return . B.pack . (++ "\n") . show $ x))
      .| mapC (CommonCmd,)
      .| mapMC (\x@(cmd, pack) -> print ("[Alice] Send packet with command " ++ show cmd ++ ", and payload: " ++ show pack) >> return x)
      .| broadcast "Alice" manager
```

与接收不同，广播不需要进行地址管理用于打开合适的网卡，因此只要名字和套接字管理器即可完成发送。测试代码中使用了拙劣的技巧，使得数据发送间有 100 毫秒的间隔。`broadcast "Alice" manager :: (Command c, MonadIO m) => ConduitT (c, ByteString) o m ()`，接收上游数据 `(c, ByteString)` 并将它们打成远程包广播出去。

