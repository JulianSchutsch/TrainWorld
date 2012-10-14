-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of TrainWorld
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   18.Sep 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   This implementation can act as a replacement for real network connections
--   in communication between tasks.
--   It uses shared memory to create anonymous pipes between client and server.

-- Usage
--   Every server created is registered with an unique address.

with Config;
with Streams;

package ClientServerStream.SMPipe is

   procedure Register;

   procedure CreateServerConfig
     (Configuration : in out Config.ConfigNode_Type;
      Address       : String_Ref;
      BufferSize    : Streams.StreamSize_Type:=1024);

   procedure CreateClientConfig
     (Configuration : in out Config.ConfigNode_Type;
      ClientAddress : String_Ref;
      ServerAddress : String_Ref);

end ClientServerStream.SMPipe;
