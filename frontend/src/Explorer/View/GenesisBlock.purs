-- | Detail page of a `genesis block` https://cardanodocs.com/technical/blocks/#genesis-block

module Explorer.View.GenesisBlock
    ( genesisBlockView
    , maxAddressInfoRows
    )
    where

import Prelude

import Data.Array (null)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cGenesis, cAddress, cAddresses, cAll, cOf, common, cLoading, cNo, cSummary, cYes, gblAddressesError, gblAddressesNotFound, gblAddressRedeemAmount, gblAddressIsRedeemed, gblAddressIsNotRedeemed, gblNotFound, gblNumberRedeemedAddresses, gblNumberAddressesToRedeem, gblNumberRedeemedAmount, gblNumberAmountToRedeem, genesisBlock) as I18nL
import Explorer.Lenses.State (currentCGenesisAddressInfos, currentCGenesisSummary, gblAddressInfosPageNumber, gblAddressInfosPagination, gblAddressInfosPaginationEditable, gblLoadingAddressInfosPagination, gblAddressInfosMaxPageNumber, genesisBlockViewState, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), CGenesisAddressInfos, GenesisBlockPagination(..), PageNumber(..), State)
import Explorer.Util.String (formatADA)
import Explorer.View.Common (currencyCSSClass, paginationView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CGenesisAddressInfo(..), CGenesisSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, cgaiCardanoAddress, cgaiGenesisAmount, cgaiIsRedeemed, cgsAmountRedeemed, cgsAmountRemaining, cgsNumRedeemed, cgsNumRemaining)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (a, div, h3, li, p, span, ul) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup ((!), (#!))
import Text.Smolder.Markup (text) as S

genesisBlockView :: State -> P.HTML Action
genesisBlockView state =
    let lang' = state ^. lang
    in
    S.div ! S.className "explorer-genesis"$ do
        S.div ! S.className "explorer-genesis__wrapper"
              $ S.div ! S.className "explorer-genesis__container" $ do
                    S.h3  ! S.className "headline"
                        $ S.text (translate (I18nL.common <<< I18nL.cGenesis) lang')
                    case state ^. currentCGenesisSummary of
                        NotAsked  -> emptyView ""
                        Loading   -> emptyView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                        Failure _ -> emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblNotFound) lang'
                        Success summary -> summaryView summary state

        S.div ! S.className "explorer-genesis__wrapper" $ do
            S.div ! S.className "explorer-genesis__container" $ do
                S.h3  ! S.className "headline"
                      $ S.text (translate (I18nL.common <<< I18nL.cAddresses) lang')
                case state ^. currentCGenesisAddressInfos of
                    NotAsked  -> emptyView ""
                    Loading   -> emptyView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                    Failure _ -> emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesError) lang'
                    Success infos -> addressInfosView infos state


emptyView :: String -> P.HTML Action
emptyView message =
    S.div ! S.className "summary-empty__container"
          $ S.text message

type SummaryRowItem =
    { key :: String
    , label :: String
    , amount :: String
    , mCurrency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem


mkSummaryItems :: State -> CGenesisSummary -> SummaryItems
mkSummaryItems state (CGenesisSummary summary) =
    let lang' = state ^. lang in
    [ { id: "0"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberRedeemedAddresses) lang'
      , amount: show $ summary ^. cgsNumRedeemed
      , mCurrency: Nothing
      }
    , { id: "2"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberAddressesToRedeem) lang'
      , amount: show $ summary ^. cgsNumRemaining
      , mCurrency: Nothing
      }
    , { id: "3"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberRedeemedAmount) lang'
      , amount: formatADA (summary ^. cgsAmountRedeemed) lang'
      , mCurrency: Just ADA
      }
    , { id: "4"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberAmountToRedeem) lang'
      , amount: formatADA (summary ^. cgsAmountRemaining) lang'
      , mCurrency: Just ADA
      }

    ]

summaryRow :: SummaryRowItem -> P.HTML Action
summaryRow item =
    S.div
        ! S.className "row row__summary"
        ! P.key item.key
        $ do
            S.div ! S.className "column column__label"
                  $ S.text item.label
            S.div ! S.className "column column__amount"
                  $ S.span  ! S.className (currencyCSSClass item.mCurrency)
                            $ S.text item.amount

summaryView :: CGenesisSummary -> State -> P.HTML Action
summaryView summary state =
    S.div ! S.className "summary-wrapper" $ do
        S.div ! S.className "summary-container" $ do
            S.h3  ! S.className "subheadline"
                  $ S.text (translate (I18nL.common <<< I18nL.cSummary) $ state ^. lang)
            S.div $ for_ (mkSummaryItems state summary) summaryRow

type AddressInfosHeaderProps =
    { label :: String
    , clazz :: String
    }

mkAddressInfosHeaderProps :: Language -> Array AddressInfosHeaderProps
mkAddressInfosHeaderProps lang =
    [ { label: translate (I18nL.common <<< I18nL.cAddress) lang
      , clazz: "hash"
      }
    , { label: translate (I18nL.genesisBlock <<< I18nL.gblAddressRedeemAmount) lang
      , clazz: "amount"
      }
    , { label: translate (I18nL.common <<< I18nL.cAddressIsRedeemed) lang
      , clazz: "redeemed"
      }
    ]

addressInfosFilterView :: State -> P.HTML Action
addressInfosFilterView state =
    let pagination = state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosPagination)
        lang' = state ^. lang
    in
    S.ul ! S.className "address-infos__filter-nav"
          $ do
          addressInfosFilterItemView
              (translate (I18nL.common <<< I18nL.cAll) lang')
              GBPaginateAllAddresses
              pagination
          addressInfosFilterItemView
              (translate (I18nL.genesisBlock <<< I18nL.gblAddressIsRedeemed) lang')
              GBPaginateRedeemedAddresses
              pagination
          addressInfosFilterItemView
              (translate (I18nL.genesisBlock <<< I18nL.gblAddressIsNotRedeemed) lang')
              GBPaginateNonRedeemedAddresses
              pagination

addressInfosFilterItemView :: String -> GenesisBlockPagination -> GenesisBlockPagination -> P.HTML Action
addressInfosFilterItemView label pagination currentPagination =
    S.li  ! S.className ("address-infos__filter-nav--item"
                            <> if (currentPagination == pagination)
                                  then " selected"
                                  else ""
                        )
          #! P.onClick (const $ GenesisBlockFilterAddressInfos pagination)
          $ S.text label


addressInfosHeaderView :: Language -> P.HTML Action
addressInfosHeaderView lang =
    S.div ! S.className "address-infos__header"
          $ for_ (mkAddressInfosHeaderProps lang) addressInfosHeaderItemView

addressInfosHeaderItemView :: AddressInfosHeaderProps -> P.HTML Action
addressInfosHeaderItemView props =
    S.div ! S.className props.clazz
          $ S.text props.label

addressInfosBodyView :: Language -> CGenesisAddressInfo -> P.HTML Action
addressInfosBodyView lang (CGenesisAddressInfo info) =
    let addrLink = toUrl $ Address (info ^. cgaiCardanoAddress) in
    S.div ! S.className "address-infos__body--row"
          $ do
          -- hash
          S.a ! S.className "address-infos__body--column hash"
              ! S.href addrLink
              #! P.onClick (Navigate addrLink)
              $ S.text (info ^. (cgaiCardanoAddress <<< _CAddress))
          -- amount
          S.div ! S.className "address-infos__body--column amount"
                $ S.span ! S.className (currencyCSSClass $ Just ADA)
                          $ S.text (formatADA (info ^. cgaiGenesisAmount) lang)
          -- is redeemed
          S.div ! S.className "address-infos__body--column redeemed"
                $ S.text (if (info ^. cgaiIsRedeemed)
                              then translate (I18nL.common <<< I18nL.cYes) lang
                              else translate (I18nL.common <<< I18nL.cNo) lang)

maxAddressInfoRows :: Int
maxAddressInfoRows = 5

addressInfosView :: CGenesisAddressInfos -> State -> P.HTML Action
addressInfosView infos state = do
    S.div
        ! S.className "address-infos__wrapper"
        $ do
            addressInfosFilterView state
            addressInfosHeaderView lang'
            S.div ! S.className "address-infos__body--wrapper"
                  $ do
                  if null infos
                      then emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesNotFound) lang'
                      else for_ infos (addressInfosBodyView lang')
                  S.div ! S.className ("address-infos__body--cover" <>  if isLoading then " show" else "")
                        $ S.p ! S.className "address-infos__body--cover-label"
                              $ S.text (translate (I18nL.common <<< I18nL.cLoading) lang')
    S.div
        ! S.className "address-infos__footer"
        $  paginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                            , currentPage: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosPageNumber)
                            , minPage: PageNumber minPagination
                            , maxPage: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosMaxPageNumber)
                            , changePageAction: GenesisBlockPaginateAddresses
                            , editable: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosPaginationEditable)
                            , editableAction: GenesisBlockEditAddressesPageNumber
                            , invalidPageAction: GenesisBlockInvalidAddressesPageNumber
                            , disabled: isLoading
                            }
    where
        lang' = state ^. lang
        isLoading = state ^. (viewStates <<< genesisBlockViewState <<< gblLoadingAddressInfosPagination)
