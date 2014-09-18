{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a
-- key rotation work flow. If the UserName field is not specified, the
-- UserName is determined implicitly based on the AWS access key ID used to
-- sign the request. Because this action works for access keys under the AWS
-- account, this API can be used to manage root credentials even if the AWS
-- account has no associated users. For information about rotating keys, see
-- Managing Keys and Certificates in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=UpdateAccessKey &UserName=Bob
-- &AccessKeyId=AKIAIOSFODNN7EXAMPLE &Status=Inactive &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateAccessKey
    (
    -- * Request
      UpdateAccessKey
    -- ** Request constructor
    , updateAccessKey
    -- ** Request lenses
    , uakUserName
    , uakAccessKeyId
    , uakStatus

    -- * Response
    , UpdateAccessKeyResponse
    -- ** Response constructor
    , updateAccessKeyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateAccessKey = UpdateAccessKey
    { _uakUserName :: Maybe Text
    , _uakAccessKeyId :: Text
    , _uakStatus :: StatusType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccessKey' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Maybe Text@
--
-- * @AccessKeyId ::@ @Text@
--
-- * @Status ::@ @StatusType@
--
updateAccessKey :: Text -- ^ 'uakAccessKeyId'
                  -> StatusType -- ^ 'uakStatus'
                  -> UpdateAccessKey
updateAccessKey p2 p3 = UpdateAccessKey
    { _uakUserName = Nothing
    , _uakAccessKeyId = p2
    , _uakStatus = p3
    }

-- | Name of the user whose key you want to update.
uakUserName :: Lens' UpdateAccessKey (Maybe Text)
uakUserName = lens _uakUserName (\s a -> s { _uakUserName = a })

-- | The access key ID of the secret access key you want to update.
uakAccessKeyId :: Lens' UpdateAccessKey Text
uakAccessKeyId = lens _uakAccessKeyId (\s a -> s { _uakAccessKeyId = a })

-- | The status you want to assign to the secret access key. Active means the
-- key can be used for API calls to AWS, while Inactive means the key cannot
-- be used.
uakStatus :: Lens' UpdateAccessKey StatusType
uakStatus = lens _uakStatus (\s a -> s { _uakStatus = a })

instance ToQuery UpdateAccessKey where
    toQuery = genericQuery def

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccessKeyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateAccessKeyResponse :: UpdateAccessKeyResponse
updateAccessKeyResponse = UpdateAccessKeyResponse

instance AWSRequest UpdateAccessKey where
    type Sv UpdateAccessKey = IAM
    type Rs UpdateAccessKey = UpdateAccessKeyResponse

    request = post "UpdateAccessKey"
    response _ = nullaryResponse UpdateAccessKeyResponse
