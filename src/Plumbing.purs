module Plumbing where

-- | Mostly just Dialog invocations
type PrimaryQueues =
  { importQueues           :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
  , exportQueue            :: Q.Queue (write :: Q.WRITE) Export.ExportDialog
  , newQueues              :: IOQueues Q.Queue Unit Boolean
  , settingsEditQueues     :: IOQueues Q.Queue Unit (Maybe Settings)
  , timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
  , timeScaleEditQueues    :: IOQueues Q.Queue Unit (Maybe TimeScale)
  , snackbarQueue          :: Q.Queue (write :: Q.WRITE) SnackbarContent
  }

newPrimaryQueues :: Effect PrimaryQueues
newPrimaryQueues = do
  ( importQueues :: IOQueues Q.Queue Import.ImportDialog (Maybe File)
    ) <- IOQueues.new
  ( exportQueue :: Q.Queue (write :: Q.WRITE) Export.ExportDialog
    ) <- Q.writeOnly <$> Q.new
  ( newQueues :: IOQueues Q.Queue Unit Boolean
    ) <- IOQueues.new
  ( settingsEditQueues :: IOQueues Q.Queue Unit (Maybe Settings)
    ) <- IOQueues.new
  ( timelineNameEditQueues :: IOQueues Q.Queue Unit (Maybe TimelineName)
    ) <- IOQueues.new
  ( timeScaleEditQueues :: IOQueues Q.Queue Unit (Maybe TimeScale)
    ) <- IOQueues.new

  -- sudden messages and notices
  ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
    ) <- Q.writeOnly <$> Q.new

  pure
    { importQueues
    , exportQueues
    , newQueues
    , settingsEditQueues
    , timelineNameEditQueues
    , timeScaleEditQueues
    , snackbarQueue
    }
